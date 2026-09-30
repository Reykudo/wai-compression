{-# LANGUAGE OverloadedStrings #-}

{- | Codec-independent HTTP body transformations for WAI. Response compression
requires an explicit application policy; request decoding is bounded and
validates the complete body before the application receives it.
-}
module Network.Wai.Middleware.Compression (
    CompressionSettings (..),
    ResponseSettings (..),
    defaultCompressionSettings,
    defaultResponseSettings,
    compression,
    decompressRequests,
    compressResponses,
    decompressResponses,
    compressRequests,
    defaultCompressibleMime,
    module Network.Wai.Middleware.Compression.Codec,
    module Network.Wai.Middleware.Compression.Policy,
) where

import Control.Exception (throwIO, try)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Internal (Response (..))
import Network.Wai.Middleware.Compression.Buffer (newCollector)
import Network.Wai.Middleware.Compression.Codec
import Network.Wai.Middleware.Compression.Internal
import Network.Wai.Middleware.Compression.Negotiation
import Network.Wai.Middleware.Compression.Policy

-- | Settings for @compression@: incoming decoding limits and outgoing policy.
data CompressionSettings = CompressionSettings
    { requestBodyLimits :: !BodyLimits
    , responseSettings :: ResponseSettings
    }

{- | Response safety gates and encoding policy. @responsePolicy@ must explicitly
allow a response; the MIME predicate and other safety gates still apply.
@responseEncodingPolicy@ only chooses order and profile among registered
codecs, without overriding client quality values. @responseMinimumSize@ is a
byte threshold; unknown builder bodies are inspected only up to that threshold.
File compression and sensitive-response compression require separate opt-ins.
-}
data ResponseSettings = ResponseSettings
    { responsePolicy :: Request -> Response -> Bool
    , responseEncodingPolicy :: EncodingPolicy
    , responseMimePredicate :: BS.ByteString -> Bool
    , responseMinimumSize :: !Integer
    , responseCompressFiles :: !Bool
    , responseAllowSensitive :: !Bool
    }

{- | Compression disabled, registration-order preference, Balanced profile,
conservative MIME selection, 1 KiB threshold, and no file or sensitive opt-ins.
-}
defaultResponseSettings :: ResponseSettings
defaultResponseSettings =
    ResponseSettings
        { responsePolicy = \_ _ -> False
        , responseEncodingPolicy = defaultEncodingPolicy
        , responseMimePredicate = defaultCompressibleMime
        , responseMinimumSize = 1024
        , responseCompressFiles = False
        , responseAllowSensitive = False
        }

{- | Bounded incoming decoding with @defaultBodyLimits@ and outgoing compression
disabled by @defaultResponseSettings@.
-}
defaultCompressionSettings :: CompressionSettings
defaultCompressionSettings = CompressionSettings defaultBodyLimits defaultResponseSettings

{- | Decode incoming requests before invoking the application, then negotiate
streaming response compression. Register only the codecs the application needs.
Configuration errors are exceptions; malformed requests receive HTTP errors.
-}
compression :: CompressionSettings -> [Codec] -> Middleware
compression settings codecs =
    decompressRequests (requestBodyLimits settings) codecs
        . compressResponses (responseSettings settings) codecs

{- | Decode declared request content codings, including chunked uploads. Encoded
and decoded bodies are buffered within limits and completely validated before
the application runs; this is not unbuffered decoded-request delivery.
Identity bodies pass through and require a separate application size limit.
-}
decompressRequests :: BodyLimits -> [Codec] -> Middleware
decompressRequests limits codecs application request respond = do
    checkConfiguration limits codecs
    prepared <- try $ do
        names <- either throwIO pure (parseContentEncoding limits (requestHeaders request))
        if null names
            then pure request
            else do
                when
                    (noTransform (requestHeaders request))
                    (throwIO (UnsupportedContentEncoding "no-transform"))
                checkSupported codecs names
                decoded <- withinDeadline limits $ do
                    encoded <- readBounded (maxEncodedBytes limits) (getRequestBodyChunk request)
                    decodeLayers limits codecs names encoded
                replaceRequestBody request Nothing decoded
    case prepared of
        Left failure -> respond (requestFailure codecs failure)
        Right decodedRequest -> application decodedRequest respond

{- | Negotiate and stream eligible responses while preserving flush boundaries.
Client quality values take precedence over server order; excluded codings are
never enabled by a policy. Transformed representation headers are normalized.
-}
compressResponses :: ResponseSettings -> [Codec] -> Middleware
compressResponses settings codecs application request respond = do
    either throwIO pure (validateCodecs codecs)
    when
        (responseMinimumSize settings < 0)
        (throwIO (InvalidConfiguration "responseMinimumSize must not be negative"))
    application request $ \response ->
        if rawResponse response || alreadyEncoded response
            then respond response
            else
                if bodyless request response
                    then respond (mapResponseHeaders addAcceptEncodingVary response)
                    else do
                        let headers = requestHeaders request
                            eligible = shouldCompress settings request response
                            available = map codecName codecs
                            varyResponse = mapResponseHeaders addAcceptEncodingVary response
                        case negotiateEncoding available headers of
                            Left InvalidAcceptEncoding -> respond (failureResponse status400 "Invalid Accept-Encoding" [])
                            Left NoAcceptableEncoding -> respond (failureResponse status406 "No acceptable content encoding" [(hVary, "Accept-Encoding")])
                            Right Nothing -> respond varyResponse
                            Right (Just _) -> do
                                plan <-
                                    if eligible
                                        then either throwIO pure (validateEncodingPlan available (responseEncodingPolicy settings request response available))
                                        else pure (EncodingPlan [] Balanced)
                                case negotiateEncoding (preferredEncodings plan) headers of
                                    Left InvalidAcceptEncoding -> respond (failureResponse status400 "Invalid Accept-Encoding" [])
                                    Left NoAcceptableEncoding -> respond (failureResponse status406 "No acceptable content encoding" [(hVary, "Accept-Encoding")])
                                    Right Nothing -> respond varyResponse
                                    Right (Just name) -> case filter ((== name) . codecName) codecs of
                                        codec : _ -> respond (encodeResponse (compressionProfile plan) codec varyResponse)
                                        [] -> throwIO (InvalidConfiguration "negotiation selected an unavailable codec")

{- | Explicit proxy-side response decoding. Encoded responses are buffered and
validated before headers are sent. Invalid or non-transformable encoded
responses return 502; identity, raw and bodyless responses pass through.
-}
decompressResponses :: BodyLimits -> [Codec] -> Middleware
decompressResponses limits codecs application request respond = do
    checkConfiguration limits codecs
    application request $ \response ->
        if bodyless request response || rawResponse response
            then respond response
            else do
                prepared <- try $ do
                    names <- either throwIO pure (parseContentEncoding limits (responseHeaders response))
                    if null names
                        then pure response
                        else do
                            when
                                (noTransform (responseHeaders response) || responseStatus response == status206 || hasHeader hContentRange (responseHeaders response))
                                (throwIO (UnsupportedContentEncoding "non-transformable response"))
                            checkSupported codecs names
                            decoded <- withinDeadline limits $ do
                                encoded <- collectResponse limits response
                                decodeLayers limits codecs names encoded
                            pure
                                ( responseLBS
                                    (responseStatus response)
                                    ((hContentLength, BSC.pack (show (LBS.length decoded))) : stripRepresentationHeaders (responseHeaders response))
                                    decoded
                                )
                case prepared of
                    Left (_ :: CompressionError) -> respond (failureResponse status502 "Invalid compressed upstream response" [])
                    Right decodedResponse -> respond decodedResponse

{- | Explicit proxy-side request encoding, not client-negotiated upload encoding.
Plain input and encoded output are buffered within their respective limits.
Already encoded or @no-transform@ requests pass through unchanged.
-}
compressRequests :: BodyLimits -> Codec -> Middleware
compressRequests limits codec application request respond = do
    checkConfiguration limits [codec]
    if hasHeader hContentEncoding (requestHeaders request) || noTransform (requestHeaders request)
        then application request respond
        else do
            prepared <- try $ withinDeadline limits $ do
                plain <- readBounded (maxDecodedBytes limits) (getRequestBodyChunk request)
                encoded <- encodeBodyBounded (maxEncodedBytes limits) codec plain
                replaceRequestBody request (Just (codecName codec)) encoded
            case prepared of
                Left failure -> respond (requestFailure [codec] failure)
                Right encodedRequest -> application encodedRequest respond

checkConfiguration :: BodyLimits -> [Codec] -> IO ()
checkConfiguration limits codecs = do
    either throwIO pure (validateBodyLimits limits)
    either throwIO pure (validateCodecs codecs)

checkSupported :: [Codec] -> [BS.ByteString] -> IO ()
checkSupported codecs = mapM_ $ \name ->
    unless (name `elem` map codecName codecs) (throwIO (UnsupportedContentEncoding name))

replaceRequestBody :: Request -> Maybe BS.ByteString -> LBS.ByteString -> IO Request
replaceRequestBody request coding body = do
    source <- newIORef (LBS.toChunks body)
    let pull = do
            chunks <- readIORef source
            case chunks of
                [] -> pure BS.empty
                chunk : rest -> writeIORef source rest >> pure chunk
        size = LBS.length body
        headers =
            (hContentLength, BSC.pack (show size))
                : maybe [] (\name -> [(hContentEncoding, name)]) coding
                    <> filter ((/= hTransferEncoding) . fst) (stripRepresentationHeaders (requestHeaders request))
    pure
        (setRequestBodyChunks pull request)
            { requestBodyLength = KnownLength (fromIntegral size)
            , requestHeaders = headers
            }

collectResponse :: BodyLimits -> Response -> IO LBS.ByteString
collectResponse limits response = do
    (collect, finish) <- newCollector (\size -> when (size > maxEncodedBytes limits) (throwIO EncodedBodyTooLarge))
    let (_, _, withBody) = responseToStream response
        send builder = mapM_ collect (LBS.toChunks (Builder.toLazyByteString builder))
    withBody (\body -> body send (pure ()))
    finish

encodeResponse :: CompressionProfile -> Codec -> Response -> Response
encodeResponse profile codec response = responseStream (responseStatus response) headers $ \send flush -> do
    let (_, _, withBody) = responseToStream response
        producer emit flushEncoder = withBody $ \body ->
            body
                (\builder -> mapM_ emit (concatMap splitChunks (LBS.toChunks (Builder.toLazyByteString builder))))
                flushEncoder
    codecEncodeWith profile codec producer (send . Builder.byteString) flush
  where
    headers = (hContentEncoding, codecName codec) : stripRepresentationHeaders (responseHeaders response)

shouldCompress :: ResponseSettings -> Request -> Response -> Bool
shouldCompress settings request response =
    responsePolicy settings request response
        && not (noTransform (requestHeaders request) || noTransform headers)
        && not (hasHeader hRange (requestHeaders request) || hasHeader hContentRange headers || responseStatus response == status206)
        && (responseAllowSensitive settings || not (sensitive request response))
        && maybe False (responseMimePredicate settings) (lookup hContentType headers)
        && sizeEligible
        && fileEligible
  where
    headers = responseHeaders response
    fileEligible = case response of
        ResponseFile _ _ _ (Just _) -> False
        ResponseFile{} -> responseCompressFiles settings
        _ -> True
    sizeEligible = case contentLength headers of
        Just size -> size >= responseMinimumSize settings
        Nothing -> case response of
            ResponseBuilder _ _ builder -> toInteger (LBS.length (LBS.take (fromInteger (min (responseMinimumSize settings) (toInteger (maxBound :: Int)))) (Builder.toLazyByteString builder))) >= responseMinimumSize settings
            _ -> True

sensitive :: Request -> Response -> Bool
sensitive request response =
    any (`hasHeader` requestHeaders request) [hAuthorization, hCookie]
        || hasHeader hSetCookie headers
        || any
            (\token -> lower (trim (BSC.takeWhile (/= '=') token)) `elem` ["private", "no-store"])
            [token | (name, value) <- headers, name == hCacheControl, token <- BSC.split ',' value]
  where
    headers = responseHeaders response

{- | Conservative MIME allowlist for textual and structured formats. MIME
selection is not a security decision: the application must still opt in via
@responsePolicy@ and review secrets and user-controlled reflection.
-}
defaultCompressibleMime :: BS.ByteString -> Bool
defaultCompressibleMime value =
    mime /= "text/event-stream"
        && ( "text/" `BS.isPrefixOf` mime
                || mime `elem` ["application/json", "application/javascript", "application/xml", "application/xhtml+xml", "image/svg+xml"]
                || "+json" `BS.isSuffixOf` mime
                || "+xml" `BS.isSuffixOf` mime
           )
  where
    mime = lower (trim (BSC.takeWhile (/= ';') value))

bodyless :: Request -> Response -> Bool
bodyless request response = requestMethod request == methodHead || code < 200 || code `elem` [204, 205, 304]
  where
    code = statusCode (responseStatus response)

rawResponse :: Response -> Bool
rawResponse ResponseRaw{} = True
rawResponse _ = False

alreadyEncoded :: Response -> Bool
alreadyEncoded = hasHeader hContentEncoding . responseHeaders

hasHeader :: HeaderName -> [Header] -> Bool
hasHeader name = any ((== name) . fst)

requestFailure :: [Codec] -> CompressionError -> Response
requestFailure codecs failure = case failure of
    UnsupportedContentEncoding _ ->
        failureResponse
            status415
            "Unsupported content encoding"
            [(hAcceptEncoding, BSC.intercalate ", " (map codecName codecs))]
    EncodedBodyTooLarge -> tooLarge
    DecodedBodyTooLarge -> tooLarge
    ExpansionRatioExceeded -> tooLarge
    TooManyEncodingLayers -> tooLarge
    BodyTimeout -> failureResponse status408 "Compressed body processing timed out" []
    _ -> failureResponse status400 "Invalid compressed request body" []
  where
    tooLarge = failureResponse status413 "Body exceeds configured limits" []

failureResponse :: Status -> LBS.ByteString -> [Header] -> Response
failureResponse status message headers =
    responseLBS
        status
        ((hContentType, "text/plain; charset=utf-8") : (hCacheControl, "no-store") : headers)
        message

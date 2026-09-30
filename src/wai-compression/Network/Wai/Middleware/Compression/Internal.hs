{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.Compression.Internal (
    decodeLayers,
    parseContentEncoding,
    readBounded,
    stripRepresentationHeaders,
    noTransform,
    lower,
    trim,
    withinDeadline,
    validateCodecs,
    contentLength,
) where

import Control.Exception (throwIO)
import Control.Monad (foldM, when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.List (nub)
import Network.HTTP.Types (Header, hCacheControl, hContentEncoding, hContentLength)
import Network.HTTP.Types.Header (hTransferEncoding)
import Network.Wai.Middleware.Compression.Buffer (newCollector)
import Network.Wai.Middleware.Compression.Codec
import System.Timeout (timeout)

validateCodecs :: [Codec] -> Either CompressionError ()
validateCodecs codecs
    | names == nub names = Right ()
    | otherwise = Left (InvalidConfiguration "duplicate codec names")
  where
    names = map codecName codecs

parseContentEncoding :: BodyLimits -> [Header] -> Either CompressionError [BS.ByteString]
parseContentEncoding limits headers
    | null values = Right []
    | any (BS.null . trim) values = Left MalformedContentEncoding
    | sum (map BS.length values) > 8192 = Left MalformedContentEncoding
    | any (not . isToken) names || any (== "*") names = Left MalformedContentEncoding
    | length names > maxEncodingLayers limits = Left TooManyEncodingLayers
    | otherwise = Right (filter (/= "identity") names)
  where
    values = [value | (name, value) <- headers, name == hContentEncoding]
    names = map (lower . trim) (concatMap (BSC.split ',') values)

decodeLayers :: BodyLimits -> [Codec] -> [BS.ByteString] -> LBS.ByteString -> IO LBS.ByteString
decodeLayers limits codecs names body = do
    selected <- mapM findCodec (reverse names)
    let originalSize = toInteger (LBS.length body)
    foldM (\input codec -> decodeBodyWithSize limits originalSize codec input) body selected
  where
    findCodec name = case filter ((== name) . codecName) codecs of
        codec : _ -> pure codec
        [] -> throwIO (UnsupportedContentEncoding name)

readBounded :: Integer -> IO BS.ByteString -> IO LBS.ByteString
readBounded limit pull = do
    (emit, finish) <- newCollector (\size -> when (size > limit) (throwIO EncodedBodyTooLarge))
    let loop = do
            chunk <- pull
            if BS.null chunk then finish else emit chunk >> loop
    loop

stripRepresentationHeaders :: [Header] -> [Header]
stripRepresentationHeaders =
    filter
        ( \(name, _) ->
            name
                `notElem` [hContentEncoding, hContentLength, hTransferEncoding, "Content-MD5", "Digest", "Content-Digest", "Repr-Digest", "ETag", "Accept-Ranges"]
        )

noTransform :: [Header] -> Bool
noTransform headers =
    any
        ((== "no-transform") . lower . trim . BSC.takeWhile (/= '='))
        [token | (name, value) <- headers, name == hCacheControl, token <- BSC.split ',' value]

contentLength :: [Header] -> Maybe Integer
contentLength headers = case [value | (name, value) <- headers, name == hContentLength] of
    [value]
        | not (BS.null value) && BS.length value <= 20 && BS.all (\byte -> byte >= 48 && byte <= 57) value ->
            Just (BS.foldl' (\total byte -> total * 10 + toInteger byte - 48) 0 value)
    _ -> Nothing

withinDeadline :: BodyLimits -> IO result -> IO result
withinDeadline limits action = do
    either throwIO pure (validateBodyLimits limits)
    outcome <- timeout (bodyTimeoutMicros limits) action
    maybe (throwIO BodyTimeout) pure outcome

lower :: BS.ByteString -> BS.ByteString
lower = BSC.map (\character -> if character >= 'A' && character <= 'Z' then toEnum (fromEnum character + 32) else character)

trim :: BS.ByteString -> BS.ByteString
trim = BSC.dropWhileEnd space . BSC.dropWhile space
  where
    space character = character == ' ' || character == '\t'

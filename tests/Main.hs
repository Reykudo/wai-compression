{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.Compression.Brotli qualified as ReferenceBrotli
import Codec.Compression.GZip qualified as ReferenceGzip
import Codec.Compression.Zlib qualified as ReferenceDeflate
import Codec.Compression.Zstd.Lazy qualified as ReferenceZstd
import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (AsyncException (ThreadKilled), bracket, throwIO, try)
import Control.Monad (forM_, unless, void)
import Data.Bits (shiftR, xor)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (permutations, sort)
import Data.Word (Word32, Word8)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Internal (ResponseReceived (..))
import Network.Wai.Middleware.Compression
import Network.Wai.Middleware.Compression.Brotli (brotli, brotliWith)
import Network.Wai.Middleware.Compression.Encoding qualified as Encoding
import Network.Wai.Middleware.Compression.Negotiation
import Network.Wai.Middleware.Compression.Zlib (deflate, deflateWith, gzip, gzipWith)
import Network.Wai.Middleware.Compression.Zstd (zstd, zstdWith)
import Network.Wai.Test qualified as WaiTest
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (label)

main :: IO ()
main =
    defaultMain
        ( testGroup
            "wai-compression"
            [ encodingNameTests
            , negotiationTests
            , configurationTests
            , policyTests
            , profileTests
            , testGroup "codecs" (map codecTests codecs)
            , responseTests
            , requestTests
            , proxyTests
            , networkTests
            , chunkedTests
            , safetyTests
            ]
        )

codecs :: [Codec]
codecs = [brotli, zstd, gzip, deflate]

payload :: LBS.ByteString
payload = LBS.fromChunks (replicate 2000 "Public test payload: abcdefghijklmnopqrstuvwxyz 0123456789\n")

responseOptions :: ResponseSettings
responseOptions = defaultResponseSettings{responsePolicy = \_ _ -> True}

settings :: CompressionSettings
settings = defaultCompressionSettings{responseSettings = responseOptions}

getResponse :: Middleware -> Request -> Response -> IO WaiTest.SResponse
getResponse middleware waiRequest response =
    WaiTest.runSession
        (WaiTest.srequest (WaiTest.SRequest waiRequest LBS.empty))
        (middleware (\_ respond -> respond response))

requestWith :: [Header] -> Request
requestWith headers = defaultRequest{requestHeaders = headers}

plainResponse :: Response
plainResponse = responseLBS status200 [(hContentType, "text/plain")] payload

encodingNameTests :: TestTree
encodingNameTests =
    testGroup
        "encoding names"
        [ testGroup
            "wire tokens"
            [ testCase label (name @?= expected)
            | (label, name, expected) <-
                [ ("gzip", Encoding.gzip, "gzip")
                , ("deflate", Encoding.deflate, "deflate")
                , ("brotli", Encoding.brotli, "br")
                , ("zstd", Encoding.zstd, "zstd")
                ]
            ]
        , testCase "match adapter registrations" $
            map codecName codecs @?= [Encoding.brotli, Encoding.zstd, Encoding.gzip, Encoding.deflate]
        , testCase "usable with custom codecs" $
            forM_ [Encoding.gzip, Encoding.deflate, Encoding.brotli, Encoding.zstd] $ \name -> do
                codec <- either throwIO pure (mkBufferedCodec 4096 name id id)
                codecName codec @?= name
        ]

negotiationTests :: TestTree
negotiationTests =
    testGroup
        "negotiation"
        [ testCase label (negotiateEncoding [Encoding.brotli, Encoding.zstd, Encoding.gzip, Encoding.deflate] headers @?= expected)
        | (label, headers, expected) <-
            [ ("absent header chooses identity", [], Right Nothing)
            , ("empty header chooses identity", [(hAcceptEncoding, "")], Right Nothing)
            , ("server preference breaks ties", [(hAcceptEncoding, "gzip, br, zstd")], Right (Just "br"))
            , ("client quality precedes server preference", [(hAcceptEncoding, "br;q=0.1,gzip;q=0.8,identity;q=0")], Right (Just "gzip"))
            , ("implicit identity has quality one", [(hAcceptEncoding, "gzip;q=0.5")], Right Nothing)
            , ("wildcard", [(hAcceptEncoding, "*")], Right (Just "br"))
            , ("explicit exclusion overrides wildcard", [(hAcceptEncoding, "*;q=1,br;q=0")], Right (Just "zstd"))
            , ("wildcard zero excludes identity", [(hAcceptEncoding, "*;q=0")], Left NoAcceptableEncoding)
            , ("explicit identity overrides wildcard", [(hAcceptEncoding, "*;q=0,identity;q=0.7")], Right Nothing)
            , ("all zero returns failure", [(hAcceptEncoding, "gzip;q=0, identity;q=0")], Left NoAcceptableEncoding)
            , ("unknown algorithm", [(hAcceptEncoding, "unknown, identity;q=0")], Left NoAcceptableEncoding)
            , ("case and whitespace", [(hAcceptEncoding, " GZIP ; Q=1.000 , BR;q=0.000 ")], Right (Just "gzip"))
            , ("duplicate exclusion cannot be overridden", [(hAcceptEncoding, "gzip;q=0,gzip;q=1,identity;q=0")], Left NoAcceptableEncoding)
            , ("multiple field lines", [(hAcceptEncoding, "gzip"), (hAcceptEncoding, "br")], Right (Just "br"))
            , ("too many quality decimals fail closed", [(hAcceptEncoding, "gzip;q=0.9999,identity;q=0")], Left NoAcceptableEncoding)
            , ("quality greater than one fails closed", [(hAcceptEncoding, "gzip;q=1.001,identity;q=0")], Left NoAcceptableEncoding)
            , ("nonstandard dot quality fails closed", [(hAcceptEncoding, "gzip;q=.9,identity;q=0")], Left NoAcceptableEncoding)
            , ("extra parameter fails closed", [(hAcceptEncoding, "gzip;q=1;foo=bar,identity;q=0")], Left NoAcceptableEncoding)
            , ("negative quality fails closed", [(hAcceptEncoding, "gzip;q=-1,identity;q=0")], Left NoAcceptableEncoding)
            , ("excessive header is rejected", [(hAcceptEncoding, BS.replicate 8193 97)], Left InvalidAcceptEncoding)
            ]
        ]

configurationTests :: TestTree
configurationTests =
    testGroup
        "configuration"
        [ testCase "safe defaults do not compress" $ do
            response <-
                getResponse
                    (compression defaultCompressionSettings codecs)
                    (requestWith [(hAcceptEncoding, "br,gzip")])
                    plainResponse
            lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Nothing
            WaiTest.simpleBody response @?= payload
        , testCase "empty codec list has no hidden codec" $ do
            response <-
                getResponse
                    (compression settings [])
                    (requestWith [(hAcceptEncoding, "br,gzip")])
                    plainResponse
            WaiTest.simpleBody response @?= payload
        , testCase "codec names are validated" $ forM_ ["", "GZIP", "a b", "identity", "*", "gzip\r\nx: y"] $ \name ->
            assertLeft (mkCodec name (\_ _ _ -> pure ()) (\_ _ -> pure ()))
        , testCase "compression levels are validated" $ do
            assertLeft (gzipWith (-1))
            assertLeft (gzipWith 10)
            assertLeft (deflateWith 10)
            assertLeft (brotliWith 12)
            assertLeft (zstdWith 0)
            assertLeft (zstdWith 100000)
        , testCase "limits are validated" $
            forM_
                [ defaultBodyLimits{maxEncodedBytes = 0}
                , defaultBodyLimits{maxDecodedBytes = 0}
                , defaultBodyLimits{maxExpansionRatio = 0}
                , defaultBodyLimits{expansionRatioSlack = -1}
                , defaultBodyLimits{maxEncodingLayers = 0}
                , defaultBodyLimits{bodyTimeoutMicros = 0}
                ]
                (assertLeft . validateBodyLimits)
        , testCase "duplicate codecs are configuration errors" $ do
            result <- try @CompressionError (getResponse (compressResponses responseOptions [gzip, gzip]) defaultRequest plainResponse)
            case result of
                Left (InvalidConfiguration _) -> pure ()
                _ -> assertFailure "expected configuration error"
        , testCase "pure buffered constructor" $ do
            codec <- either throwIO pure (mkBufferedCodec 4096 "reverse" BS.reverse BS.reverse)
            encoded <- encodeBody codec "abcdef"
            encoded @?= "fedcba"
            decodeBody defaultBodyLimits codec encoded >>= (@?= "abcdef")
        , testCase "buffered codec input quota" $ do
            codec <- either throwIO pure (mkBufferedCodec 4 "reverse" BS.reverse BS.reverse)
            try @CompressionError (encodeBody codec "abcde") >>= (@?= Left EncodedBodyTooLarge)
        , testCase "buffered codec output quota" $ do
            codec <- either throwIO pure (mkBufferedCodec 4 "expander" (<> "12345") id)
            try @CompressionError (encodeBody codec "a") >>= (@?= Left DecodedBodyTooLarge)
        ]

policyTests :: TestTree
policyTests =
    testGroup
        "encoding policy"
        [ testCase "default keeps registered order and levels" $
            defaultEncodingPolicy defaultRequest plainResponse names @?= EncodingPlan names Balanced
        , testCase "unknown size uses fast profile without inspecting body" $ do
            let response = responseLBS status200 [(hContentType, "text/html")] (error "policy must not evaluate body")
            recommendedPolicy defaultRequest response names @?= EncodingPlan [Encoding.zstd, Encoding.brotli, Encoding.gzip, Encoding.deflate] Fast
        , testCase "known size is header metadata only" $ do
            let response = responseLBS status200 [(hContentType, "text/html"), (hContentLength, "65536")] (error "body must remain lazy")
            responseInfo response @?= ResponseInfo (Just "text/html") (Just 65536)
            captured <- headersOnly recommendedOptions codecs (requestWith [(hAcceptEncoding, "br,zstd,gzip")]) response
            lookup hContentEncoding captured @?= Just Encoding.brotli
        , testCase "invalid or ambiguous size hints are ignored" $
            forM_ [[(hContentLength, "")], [(hContentLength, "-1")], [(hContentLength, "12x")], [(hContentLength, "1"), (hContentLength, "2")], [(hContentLength, BS.replicate 8192 57)]] $ \headers ->
                responseBodySize (responseInfo (responseLBS status200 headers (error "body must stay lazy"))) @?= Nothing
        , testCase "unknown builder inspects at most its threshold prefix" $ do
            let body = LBS.fromChunks (BS.replicate 65536 97 : error "lazy tail must not be forced")
                response = responseLBS status200 [(hContentType, "text/plain")] body
            captured <- headersOnly recommendedOptions codecs (requestWith [(hAcceptEncoding, "br,zstd,gzip")]) response
            lookup hContentEncoding captured @?= Just Encoding.zstd
        , testCase "identity negotiation does not inspect builder or policy" $ do
            let options = responseOptions{responseEncodingPolicy = \_ _ _ -> error "policy must not be invoked"}
                response = responseLBS status200 [(hContentType, "text/plain")] (error "identity body must not be inspected")
            forM_ [[], [(hAcceptEncoding, "gzip;q=0.5")], [(hAcceptEncoding, "")]] $ \headers -> do
                captured <- headersOnly options codecs (requestWith headers) response
                lookup hContentEncoding captured @?= Nothing
        , testCase "profile size boundary" $
            forM_ [(Nothing, Fast), (Just 0, Fast), (Just 16383, Fast), (Just 16384, Balanced), (Just 1048576, Balanced)] $ \(size, expected) ->
                compressionProfile (recommendedPolicyFor (ResponseInfo (Just "application/json") size) names) @?= expected
        , testCase "MIME normalization and structured suffixes" $ do
            forM_ ["TEXT/HTML; charset=UTF-8", "text/css", "application/javascript", "text/javascript", "image/svg+xml", "application/atom+xml"] $ \mime ->
                take 1 (preferredEncodings (recommendedPolicyFor (ResponseInfo (Just mime) (Just 65536)) names)) @?= [Encoding.brotli]
            forM_ ["application/json", "application/problem+json", "text/plain; charset=utf-8"] $ \mime ->
                take 1 (preferredEncodings (recommendedPolicyFor (ResponseInfo (Just mime) (Just 65536)) names)) @?= [Encoding.zstd]
        , testCase "unknown MIME preserves server preference" $
            forM_ [Nothing, Just "application/octet-stream", Just "font/woff2"] $ \mime ->
                preferredEncodings (recommendedPolicyFor (ResponseInfo mime (Just 65536)) names) @?= names
        , testCase "unavailable codecs are not introduced" $ do
            recommendedPolicyFor (ResponseInfo (Just "text/html") (Just 65536)) [Encoding.gzip] @?= EncodingPlan [Encoding.gzip] Balanced
            recommendedPolicyFor (ResponseInfo (Just "text/html") (Just 65536)) [] @?= EncodingPlan [] Balanced
        , testCase "custom codecs are retained in their original order" $
            preferredEncodings (recommendedPolicyFor (ResponseInfo (Just "text/html") (Just 65536)) ["custom-b", Encoding.gzip, "custom-a", Encoding.brotli])
                @?= [Encoding.brotli, Encoding.gzip, "custom-b", "custom-a"]
        , testCase "recommendation is a permutation" $
            forM_ (permutations names) $ \available ->
                forM_ [Nothing, Just "text/html", Just "application/json", Just "text/plain", Just "application/unknown"] $ \mime ->
                    forM_ [Nothing, Just 4096, Just 65536] $ \size ->
                        sort (preferredEncodings (recommendedPolicyFor (ResponseInfo mime size) available)) @?= sort available
        , testCase "invalid plans reject missing duplicate and unknown codecs" $
            forM_ [[], [Encoding.gzip, Encoding.gzip], ["unknown"], names <> ["unknown"]] $ \preference ->
                assertLeft (validateEncodingPlan names (EncodingPlan preference Fast))
        , testCase "middleware rejects an invalid custom plan" $ do
            let options = responseOptions{responseEncodingPolicy = \_ _ _ -> EncodingPlan ["unknown"] Fast}
            result <- try @CompressionError (getResponse (compressResponses options codecs) (requestWith [(hAcceptEncoding, "gzip")]) plainResponse)
            case result of
                Left (InvalidConfiguration _) -> pure ()
                _ -> assertFailure "expected invalid policy configuration"
        , testCase "policy cannot opt a protected response into compression" $ do
            let options = recommendedOptions{responseEncodingPolicy = \_ _ _ -> error "protected response must not invoke policy"}
            response <- getResponse (compressResponses options codecs) (requestWith [(hAcceptEncoding, "br,zstd,gzip"), (hCookie, "session=secret")]) plainResponse
            lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Nothing
        , testCase "recommended policy does not enable compression by itself" $ do
            response <- getResponse (compressResponses (defaultResponseSettings{responseEncodingPolicy = recommendedPolicy}) codecs) (requestWith [(hAcceptEncoding, "br,zstd,gzip")]) plainResponse
            lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Nothing
        , testCase "client quality beats recommendation" $ do
            response <- getResponse (compressResponses recommendedOptions codecs) (requestWith [(hAcceptEncoding, "zstd;q=0.5,br;q=1")]) plainResponse
            lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Just Encoding.brotli
        , testCase "client exclusion beats recommendation" $ do
            response <- getResponse (compressResponses recommendedOptions codecs) (requestWith [(hAcceptEncoding, "zstd;q=0,br")]) plainResponse
            lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Just Encoding.brotli
        , testCase "implicit q is one and policy breaks ties" $ do
            response <- getResponse (compressResponses recommendedOptions codecs) (requestWith [(hAcceptEncoding, "gzip,br,zstd")]) plainResponse
            lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Just Encoding.zstd
        , testCase "explicit client quality ties use policy" $ do
            response <- getResponse (compressResponses recommendedOptions codecs) (requestWith [(hAcceptEncoding, "gzip;q=0.8,br;q=0.8,zstd;q=0.8,identity;q=0")]) plainResponse
            lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Just Encoding.zstd
        , testProperty "recommendation preserves maximum client quality" $ \(weights :: [NonNegative Int]) ->
            let qualities = zip names (take (length names) (map (\(NonNegative weight) -> weight `mod` 1001) weights <> repeat 0))
                render quality = if quality == 1000 then "1" else "0." <> BSC.pack (replicate (3 - length digits) '0' <> digits)
                  where
                    digits = show quality
                headers = [(hAcceptEncoding, BSC.intercalate "," ("identity;q=0" : [name <> ";q=" <> render quality | (name, quality) <- qualities]))]
                preference = preferredEncodings (recommendedPolicyFor (ResponseInfo (Just "text/html") (Just 65536)) names)
             in case negotiateEncoding preference headers of
                    Right (Just name) -> encodingQuality name headers == Right (maximum (0 : map snd qualities))
                    Left NoAcceptableEncoding -> all ((== 0) . snd) qualities
                    _ -> False
        , testCase "selected profile reaches the encoder" $ do
            observed <- newIORef Nothing
            let encoder profile producer emit flush = writeIORef observed (Just profile) >> producer emit flush
                options = responseOptions{responseEncodingPolicy = \_ _ available -> EncodingPlan available Compact}
            codec <- either throwIO pure (mkAdaptiveCodec "trace" encoder (\_ _ -> pure ()))
            response <- getResponse (compressResponses options [codec]) (requestWith [(hAcceptEncoding, "trace")]) plainResponse
            WaiTest.simpleBody response @?= payload
            readIORef observed >>= (@?= Just Compact)
        ]
  where
    names = map codecName codecs

recommendedOptions :: ResponseSettings
recommendedOptions = responseOptions{responseEncodingPolicy = recommendedPolicy}

headersOnly :: ResponseSettings -> [Codec] -> Request -> Response -> IO [Header]
headersOnly options registered request response = do
    captured <- newIORef []
    void (compressResponses options registered (\_ respond -> respond response) request (\result -> writeIORef captured (responseHeaders result) >> pure ResponseReceived))
    readIORef captured

encodeWithProfile :: CompressionProfile -> Codec -> LBS.ByteString -> IO LBS.ByteString
encodeWithProfile profile codec body = do
    fixed <- either throwIO pure (mkCodec (codecName codec) (codecEncodeWith profile codec) (codecDecode codec))
    encodeBody fixed body

profileTests :: TestTree
profileTests =
    testGroup "compression profiles" $
        [ testCase (BSC.unpack (codecName codec) <> " " <> show profile) $ do
            fixed <- either throwIO pure (make level)
            encoded <- encodeWithProfile profile codec payload
            encodeBody fixed payload >>= (@?= encoded)
            decodeBody defaultBodyLimits codec encoded >>= (@?= payload)
        | (codec, make, levels) <- [(gzip, gzipWith, [1, 6, 9]), (deflate, deflateWith, [1, 6, 9]), (brotli, brotliWith, [3, 5, 6]), (zstd, zstdWith, [1, 3, 6])]
        , (profile, level) <- zip [Fast, Balanced, Compact] levels
        ]
            <> [ testCase ("fixed level ignores profile " <> BSC.unpack (codecName codec)) $ do
                    fixed <- either throwIO pure (make 2)
                    expected <- encodeBody fixed payload
                    forM_ [Fast, Balanced, Compact] $ \profile -> encodeWithProfile profile fixed payload >>= (@?= expected)
               | (codec, make) <- [(gzip, gzipWith), (deflate, deflateWith), (brotli, brotliWith), (zstd, zstdWith)]
               ]
            <> [ testCase ("flush and cancellation " <> BSC.unpack (codecName codec) <> " " <> show profile) $ do
                    chunks <- newIORef []
                    flushes <- newIORef (0 :: Int)
                    codecEncodeWith profile codec (\send flush -> send "first" >> flush >> send "second" >> flush) (\chunk -> modifyIORef' chunks (chunk :)) (modifyIORef' flushes (+ 1))
                    encoded <- LBS.fromChunks . reverse <$> readIORef chunks
                    decodeBody defaultBodyLimits codec encoded >>= (@?= "firstsecond")
                    readIORef flushes >>= (@?= 2)
                    cancelled <- try @AsyncException (codecEncodeWith profile codec (\_ _ -> throwIO ThreadKilled) (\_ -> pure ()) (pure ()))
                    case cancelled of
                        Left ThreadKilled -> pure ()
                        _ -> assertFailure "encoder swallowed cancellation"
               | codec <- codecs
               , profile <- [Fast, Balanced, Compact]
               ]
            <> [ testCase "shared codecs keep concurrent profile state isolated" $ do
                    pending <-
                        mapM
                            ( \(codec, profile) -> do
                                result <- newEmptyMVar
                                void (forkFinally (encodeWithProfile profile codec payload >>= decodeBody defaultBodyLimits codec) (putMVar result))
                                pure result
                            )
                            [(codec, profile) | codec <- codecs, profile <- [Fast, Balanced, Compact]]
                    forM_ pending $ \result -> takeMVar result >>= either throwIO (@?= payload)
               ]

codecTests :: Codec -> TestTree
codecTests codec =
    testGroup
        (BSC.unpack (codecName codec))
        [ testCase "large roundtrip" $ encodeBody codec payload >>= decodeBody defaultBodyLimits codec >>= (@?= payload)
        , testCase "empty roundtrip" $ encodeBody codec LBS.empty >>= decodeBody defaultBodyLimits codec >>= (@?= LBS.empty)
        , testCase "byte-at-a-time decoding" $ do
            encoded <- encodeBody codec payload
            let fragmented = LBS.fromChunks (map BS.singleton (LBS.unpack encoded))
            decodeBody defaultBodyLimits codec fragmented >>= (@?= payload)
        , testCase "truncation is rejected" $ do
            encoded <- encodeBody codec payload
            expectInvalid (decodeBody defaultBodyLimits codec (LBS.take (LBS.length encoded - 1) encoded))
        , testCase "garbage is rejected" $ expectInvalid (decodeBody defaultBodyLimits codec "this is not a compressed body")
        , testCase "empty input is not an empty frame" $ expectInvalid (decodeBody defaultBodyLimits codec LBS.empty)
        , testCase "trailing data is rejected" $ do
            encoded <- encodeBody codec payload
            expectInvalid (decodeBody defaultBodyLimits codec (encoded <> "trailing garbage"))
        , testCase "encoded quota" $ do
            encoded <- encodeBody codec payload
            try @CompressionError (decodeBody (defaultBodyLimits{maxEncodedBytes = 1}) codec encoded) >>= (@?= Left EncodedBodyTooLarge)
        , testCase "decoded quota" $ do
            encoded <- encodeBody codec payload
            try @CompressionError (decodeBody (defaultBodyLimits{maxDecodedBytes = 100}) codec encoded) >>= (@?= Left DecodedBodyTooLarge)
        , testCase "expansion quota" $ do
            encoded <- encodeBody codec payload
            try @CompressionError (decodeBody (defaultBodyLimits{maxExpansionRatio = 1, expansionRatioSlack = 0}) codec encoded)
                >>= (@?= Left ExpansionRatioExceeded)
        , testCase "quota permits exact boundary" $ do
            encoded <- encodeBody codec payload
            decodeBody (defaultBodyLimits{maxEncodedBytes = toInteger (LBS.length encoded), maxDecodedBytes = toInteger (LBS.length payload)}) codec encoded
                >>= (@?= payload)
        , testCase "reference decoder reads our output" $ do
            encoded <- encodeBody codec payload
            referenceDecode codec encoded @?= payload
        , testCase "our decoder reads reference output" $
            decodeBody defaultBodyLimits codec (referenceEncode codec payload) >>= (@?= payload)
        , testCase "WAI response negotiation and decoding" $ do
            response <-
                getResponse
                    (compression settings [codec])
                    (requestWith [(hAcceptEncoding, codecName codec)])
                    plainResponse
            lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Just (codecName codec)
            decodeBody defaultBodyLimits codec (WaiTest.simpleBody response) >>= (@?= payload)
        , testCase "flush emits compressed bytes before network flush" $ do
            events <- newIORef ([] :: [Bool])
            captured <- newIORef []
            codecEncode
                codec
                (\send flush -> send "first event" >> flush >> send "second event" >> flush)
                (\chunk -> modifyIORef' events (False :) >> modifyIORef' captured (chunk :))
                (modifyIORef' events (True :))
            ordered <- reverse <$> readIORef events
            unless (False `elem` takeWhile not ordered) (assertFailure "flush did not produce bytes")
            length (filter id ordered) @?= 2
            compressed <- LBS.fromChunks . reverse <$> readIORef captured
            decodeBody defaultBodyLimits codec compressed >>= (@?= "first eventsecond event")
        , testProperty "binary roundtrip" $ \(bytes :: [Word8]) -> ioProperty $ do
            let body = LBS.pack bytes
            encoded <- encodeBody codec body
            decoded <- decodeBody defaultBodyLimits codec encoded
            pure (decoded === body)
        ]

referenceEncode :: Codec -> LBS.ByteString -> LBS.ByteString
referenceEncode codec = case codecName codec of
    "gzip" -> ReferenceGzip.compress
    "deflate" -> ReferenceDeflate.compress
    "br" -> ReferenceBrotli.compress
    _ -> ReferenceZstd.compress 3

referenceDecode :: Codec -> LBS.ByteString -> LBS.ByteString
referenceDecode codec = case codecName codec of
    "gzip" -> ReferenceGzip.decompress
    "deflate" -> ReferenceDeflate.decompress
    "br" -> ReferenceBrotli.decompress
    _ -> ReferenceZstd.decompress

responseTests :: TestTree
responseTests =
    testGroup "responses" $
        [ testCase "identity response gets Vary" $ do
            response <- getResponse (compressResponses responseOptions codecs) defaultRequest plainResponse
            lookup hVary (WaiTest.simpleHeaders response) @?= Just "Accept-Encoding"
        , testCase "Vary merges multiple fields without duplicates" $ do
            let original = [(hVary, "Origin"), (hVary, "User-Agent")]
            addAcceptEncodingVary original @?= [(hVary, "Origin, User-Agent, Accept-Encoding")]
            addAcceptEncodingVary [(hVary, "accept-encoding, Origin")] @?= [(hVary, "accept-encoding, Origin")]
            addAcceptEncodingVary [(hVary, "*")] @?= [(hVary, "*")]
        , testCase "representation metadata is removed" $ do
            let headers =
                    [ (hContentType, "text/plain")
                    , (hContentLength, "28500")
                    , (hTransferEncoding, "chunked")
                    , (hETag, "\"strong\"")
                    , ("Content-MD5", "old")
                    , ("Digest", "old")
                    , ("Content-Digest", "old")
                    , ("Repr-Digest", "old")
                    , (hAcceptRanges, "bytes")
                    ]
            response <- getResponse (compressResponses responseOptions [gzip]) (requestWith [(hAcceptEncoding, "gzip")]) (responseLBS status200 headers payload)
            forM_ [hContentLength, hTransferEncoding, hETag, "Content-MD5", "Digest", "Content-Digest", "Repr-Digest", hAcceptRanges] $ \name ->
                lookup name (WaiTest.simpleHeaders response) @?= Nothing
        , testCase "small body retains length and identity" $ do
            response <-
                getResponse
                    (compressResponses responseOptions codecs)
                    (requestWith [(hAcceptEncoding, "br")])
                    (responseLBS status200 [(hContentType, "text/plain"), (hContentLength, "5")] "small")
            lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Nothing
            lookup hContentLength (WaiTest.simpleHeaders response) @?= Just "5"
        , testCase "already encoded response is untouched" $ do
            response <-
                getResponse
                    (compressResponses responseOptions codecs)
                    (requestWith [(hAcceptEncoding, "br")])
                    (responseLBS status200 [(hContentEncoding, "gzip")] "existing")
            WaiTest.simpleBody response @?= "existing"
            lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Just "gzip"
        , testCase "406 when identity and all available codecs are forbidden" $ do
            response <-
                getResponse
                    (compressResponses responseOptions [gzip])
                    (requestWith [(hAcceptEncoding, "br, identity;q=0")])
                    plainResponse
            WaiTest.simpleStatus response @?= status406
        , testCase "406 when required encoding cannot transform response" $ do
            response <-
                getResponse
                    (compressResponses defaultResponseSettings [gzip])
                    (requestWith [(hAcceptEncoding, "gzip, identity;q=0")])
                    plainResponse
            WaiTest.simpleStatus response @?= status406
        , testCase "oversized negotiation header returns 400" $ do
            response <-
                getResponse
                    (compressResponses responseOptions codecs)
                    (requestWith [(hAcceptEncoding, BS.replicate 8193 97)])
                    plainResponse
            WaiTest.simpleStatus response @?= status400
        , testCase "stream without Content-Length" $ do
            let stream = responseStream status200 [(hContentType, "text/plain")] $ \send flush -> do
                    send (Builder.lazyByteString payload)
                    flush
                    send (Builder.byteString "tail")
            response <- getResponse (compressResponses responseOptions [brotli]) (requestWith [(hAcceptEncoding, "br")]) stream
            decodeBody defaultBodyLimits brotli (WaiTest.simpleBody response) >>= (@?= payload <> "tail")
        , testCase "SSE is excluded from default MIME policy" $ defaultCompressibleMime "text/event-stream" @?= False
        , testCase "structured MIME types" $ forM_ ["application/problem+json", "Application/JSON; charset=utf-8", "image/svg+xml"] $ \mime ->
            defaultCompressibleMime mime @?= True
        , testCase "file opt-in" $ withFile payload $ \path -> do
            let file = responseFile status200 [(hContentType, "text/plain")] path Nothing
            skipped <- getResponse (compressResponses responseOptions [gzip]) (requestWith [(hAcceptEncoding, "gzip")]) file
            lookup hContentEncoding (WaiTest.simpleHeaders skipped) @?= Nothing
            encoded <-
                getResponse
                    (compressResponses (responseOptions{responseCompressFiles = True}) [gzip])
                    (requestWith [(hAcceptEncoding, "gzip")])
                    file
            decodeBody defaultBodyLimits gzip (WaiTest.simpleBody encoded) >>= (@?= payload)
        , testCase "raw responses preserve their fallback" $ do
            let response = responseRaw (\_ _ -> pure ()) plainResponse
            result <- getResponse (compressResponses responseOptions [gzip]) (requestWith [(hAcceptEncoding, "gzip")]) response
            lookup hContentEncoding (WaiTest.simpleHeaders result) @?= Nothing
        ]
            <> [ testCase label $ do
                    response <-
                        getResponse
                            (compressResponses responseOptions codecs)
                            (requestWith ((hAcceptEncoding, "br,gzip") : requestExtra))
                            (responseLBS responseCode ((hContentType, mime) : responseExtra) payload)
                    lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Nothing
               | (label, requestExtra, responseExtra, responseCode, mime) <-
                    [ ("request no-transform", [(hCacheControl, "NO-TRANSFORM")], [], status200, "text/plain")
                    , ("response no-transform", [], [(hCacheControl, "public, no-transform")], status200, "text/plain")
                    , ("range request", [(hRange, "bytes=0-100")], [], status200, "text/plain")
                    , ("partial response", [], [], status206, "text/plain")
                    , ("Content-Range response", [], [(hContentRange, "bytes 0-100/200")], status200, "text/plain")
                    , ("authorization", [(hAuthorization, "Bearer secret")], [], status200, "text/plain")
                    , ("cookie", [(hCookie, "session=secret")], [], status200, "text/plain")
                    , ("Set-Cookie", [], [(hSetCookie, "session=secret")], status200, "text/plain")
                    , ("private cache control", [], [(hCacheControl, "private")], status200, "text/plain")
                    , ("no-store cache control", [], [(hCacheControl, "no-store")], status200, "text/plain")
                    , ("204", [], [], status204, "text/plain")
                    , ("205", [], [], mkStatus 205 "Reset Content", "text/plain")
                    , ("304", [], [], status304, "text/plain")
                    , ("incompressible MIME", [], [], status200, "image/png")
                    ]
               ]
            <> [ testCase "HEAD preserves representation headers" $ do
                    response <-
                        getResponse
                            (compressResponses responseOptions codecs)
                            ((requestWith [(hAcceptEncoding, "br,gzip")]){requestMethod = methodHead})
                            plainResponse
                    lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Nothing
               ]

requestTests :: TestTree
requestTests =
    testGroup "requests" $
        [ testCase "body and metadata are normalized before app" $ do
            encoded <- encodeBody gzip payload
            let application request respond = do
                    case requestBodyLength request of
                        KnownLength size -> size @?= fromIntegral (LBS.length payload)
                        ChunkedBody -> assertFailure "decoded request must have a known length"
                    lookup hContentEncoding (requestHeaders request) @?= Nothing
                    lookup hContentLength (requestHeaders request) @?= Just (BSC.pack (show (LBS.length payload)))
                    forM_ ["Digest", "Content-MD5", hTransferEncoding] $ \name -> lookup name (requestHeaders request) @?= Nothing
                    body <- strictRequestBody request
                    body @?= payload
                    getRequestBodyChunk request >>= (@?= BS.empty)
                    respond (responseLBS status200 [] body)
            response <-
                WaiTest.runSession
                    ( WaiTest.srequest
                        ( WaiTest.SRequest
                            ((requestWith [(hContentEncoding, "gzip"), (hContentLength, "1"), ("Digest", "old"), ("Content-MD5", "old"), (hTransferEncoding, "chunked")]){requestBodyLength = ChunkedBody})
                            encoded
                        )
                    )
                    (decompressRequests defaultBodyLimits codecs application)
            WaiTest.simpleBody response @?= payload
        , testCase "stacked encodings decode in reverse order" $ do
            encoded <- encodeBody gzip payload >>= encodeBody brotli
            response <- upload defaultBodyLimits [(hContentEncoding, "gzip, br")] encoded
            WaiTest.simpleStatus response @?= status200
            WaiTest.simpleBody response @?= payload
        , testCase "multiple Content-Encoding fields" $ do
            encoded <- encodeBody gzip payload >>= encodeBody brotli
            response <- upload defaultBodyLimits [(hContentEncoding, "GZIP"), (hContentEncoding, "BR")] encoded
            WaiTest.simpleBody response @?= payload
        , testCase "identity is a no-op" $ upload defaultBodyLimits [(hContentEncoding, "identity")] payload >>= (\response -> WaiTest.simpleBody response @?= payload)
        , testCase "malformed requests never invoke application" $ do
            called <- newIORef False
            response <-
                WaiTest.runSession
                    (WaiTest.srequest (WaiTest.SRequest (requestWith [(hContentEncoding, "gzip")]) "invalid"))
                    (decompressRequests defaultBodyLimits codecs (\_ respond -> writeIORef called True >> respond plainResponse))
            WaiTest.simpleStatus response @?= status400
            readIORef called >>= (@?= False)
        , testCase "unsupported codec advertises available decoders" $ do
            response <- upload defaultBodyLimits [(hContentEncoding, "snappy")] "opaque"
            WaiTest.simpleStatus response @?= status415
            lookup hAcceptEncoding (WaiTest.simpleHeaders response) @?= Just "br, zstd, gzip, deflate"
        , testCase "encoded quota returns 413" $ do
            encoded <- encodeBody gzip payload
            upload (defaultBodyLimits{maxEncodedBytes = 1}) [(hContentEncoding, "gzip")] encoded >>= (\response -> WaiTest.simpleStatus response @?= status413)
        , testCase "decoded quota returns 413" $ do
            encoded <- encodeBody gzip payload
            upload (defaultBodyLimits{maxDecodedBytes = 1}) [(hContentEncoding, "gzip")] encoded >>= (\response -> WaiTest.simpleStatus response @?= status413)
        , testCase "too many layers returns 413" $
            upload defaultBodyLimits [(hContentEncoding, "gzip,gzip,gzip,gzip")] "opaque" >>= (\response -> WaiTest.simpleStatus response @?= status413)
        , testCase "compressed no-transform request returns 415" $
            upload defaultBodyLimits [(hContentEncoding, "gzip"), (hCacheControl, "no-transform")] "opaque" >>= (\response -> WaiTest.simpleStatus response @?= status415)
        , testCase "gzip concatenated members" $ do
            first <- encodeBody gzip "first"
            second <- encodeBody gzip "second"
            decodeBody defaultBodyLimits gzip (first <> second) >>= (@?= "firstsecond")
        , testCase "Zstandard concatenated frames" $ do
            first <- encodeBody zstd "first"
            second <- encodeBody zstd "second"
            decodeBody defaultBodyLimits zstd (first <> second) >>= (@?= "firstsecond")
        , testCase "Zstandard oversized window is rejected" $
            expectInvalid (decodeBody defaultBodyLimits zstd (LBS.pack [0x28, 0xb5, 0x2f, 0xfd, 0, 0x70, 1, 0, 0]))
        , testCase "deadline covers reading before application" $ do
            status <- newIORef status200
            let request = setRequestBodyChunks (threadDelay 100000 >> pure "data") (requestWith [(hContentEncoding, "gzip")])
            void
                ( decompressRequests
                    (defaultBodyLimits{bodyTimeoutMicros = 1000})
                    [gzip]
                    (\_ respond -> respond plainResponse)
                    request
                    (\response -> writeIORef status (responseStatus response) >> pure ResponseReceived)
                )
            readIORef status >>= (@?= status408)
        , testCase "asynchronous cancellation is not converted to 400" $ do
            let request = setRequestBodyChunks (throwIO ThreadKilled) (requestWith [(hContentEncoding, "gzip")])
            result <-
                try @AsyncException
                    ( decompressRequests
                        defaultBodyLimits
                        [gzip]
                        (\_ respond -> respond plainResponse)
                        request
                        (\_ -> pure ResponseReceived)
                    )
            case result of
                Left ThreadKilled -> pure ()
                _ -> assertFailure "asynchronous cancellation was swallowed"
        ]
            <> [ testCase ("malformed coding " <> show coding) $ do
                    response <- upload defaultBodyLimits [(hContentEncoding, coding)] "opaque"
                    WaiTest.simpleStatus response @?= status400
               | coding <- ["", "gzip,", "gzip;foo=bar", "*", "gzip\r\nX-Evil: yes"]
               ]

upload :: BodyLimits -> [Header] -> LBS.ByteString -> IO WaiTest.SResponse
upload limits headers body =
    WaiTest.runSession
        (WaiTest.srequest (WaiTest.SRequest (requestWith headers) body))
        (decompressRequests limits codecs echoApplication)

echoApplication :: Application
echoApplication request respond = do
    body <- strictRequestBody request
    respond (responseLBS status200 [(hContentType, "text/plain")] body)

proxyTests :: TestTree
proxyTests =
    testGroup
        "proxy transformations"
        [ testCase "encoded upstream response is validated and normalized" $ do
            encoded <- encodeBody brotli payload
            response <-
                getResponse
                    (decompressResponses defaultBodyLimits codecs)
                    defaultRequest
                    (responseLBS status200 [(hContentType, "text/plain"), (hContentEncoding, "br"), (hETag, "old")] encoded)
            WaiTest.simpleBody response @?= payload
            lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Nothing
            lookup hETag (WaiTest.simpleHeaders response) @?= Nothing
            lookup hContentLength (WaiTest.simpleHeaders response) @?= Just (BSC.pack (show (LBS.length payload)))
        , testCase "invalid upstream body yields 502 before headers" $ do
            response <-
                getResponse
                    (decompressResponses defaultBodyLimits codecs)
                    defaultRequest
                    (responseLBS status200 [(hContentEncoding, "br")] "bad")
            WaiTest.simpleStatus response @?= status502
        , testCase "proxy request compression and decoding compose" $ do
            response <-
                WaiTest.runSession
                    (WaiTest.srequest (WaiTest.SRequest defaultRequest payload))
                    (compressRequests defaultBodyLimits gzip (decompressRequests defaultBodyLimits codecs echoApplication))
            WaiTest.simpleBody response @?= payload
        ]

chunkedTests :: TestTree
chunkedTests =
    testGroup "chunked uploads" $
        [ testGroup
            (BSC.unpack (codecName codec))
            [ testCase "fragmented roundtrip and normalized request metadata" $ do
                seen <- newIORef Nothing
                let application request respond = do
                        writeIORef seen (Just (requestBodyLength request, requestHeaders request))
                        echoApplication request respond
                testWithApplication (pure (compression settings codecs application)) $ \port -> do
                    manager <- Client.newManager Client.defaultManagerSettings
                    initial <- Client.parseRequest ("http://127.0.0.1:" <> show port <> "/echo")
                    encoded <- encodeBody codec payload
                    let request = chunkedRequest initial [(hContentEncoding, codecName codec), (hAcceptEncoding, Encoding.brotli)] (map BS.singleton (LBS.unpack encoded))
                    response <- Client.httpLbs request manager
                    Client.responseStatus response @?= status200
                    decodeBody defaultBodyLimits brotli (Client.responseBody response) >>= (@?= payload)
                    observed <- readIORef seen
                    case observed of
                        Just (bodyLength, headers) -> do
                            case bodyLength of
                                KnownLength size -> size @?= fromIntegral (LBS.length payload)
                                ChunkedBody -> assertFailure "decoded request must have a known length"
                            lookup hContentLength headers @?= Just (BSC.pack (show (LBS.length payload)))
                            lookup hTransferEncoding headers @?= Nothing
                            lookup hContentEncoding headers @?= Nothing
                        Nothing -> assertFailure "application was not called"
            , testCase "truncated upload is rejected before application" $ do
                encoded <- encodeBody codec payload
                rejectedChunked defaultBodyLimits [(hContentEncoding, codecName codec)] (LBS.take (LBS.length encoded - 1) encoded) status400
            , testCase "encoded quota applies without Content-Length" $ do
                encoded <- encodeBody codec payload
                rejectedChunked (defaultBodyLimits{maxEncodedBytes = 1}) [(hContentEncoding, codecName codec)] encoded status413
            , testCase "decoded quota applies without Content-Length" $ do
                encoded <- encodeBody codec payload
                rejectedChunked (defaultBodyLimits{maxDecodedBytes = 1}) [(hContentEncoding, codecName codec)] encoded status413
            ]
        | codec <- codecs
        ]
            <> [ testCase "unknown coding returns 415 before application" $
                    rejectedChunked defaultBodyLimits [(hContentEncoding, "unknown")] "opaque" status415
               , testCase "stacked codings roundtrip" $
                    testWithApplication (pure (compression settings codecs echoApplication)) $ \port -> do
                        manager <- Client.newManager Client.defaultManagerSettings
                        initial <- Client.parseRequest ("http://127.0.0.1:" <> show port <> "/echo")
                        encoded <- encodeBody gzip payload >>= encodeBody brotli
                        response <- Client.httpLbs (chunkedRequest initial [(hContentEncoding, "gzip,br"), (hAcceptEncoding, "identity")] (LBS.toChunks encoded)) manager
                        Client.responseStatus response @?= status200
                        Client.responseBody response @?= payload
               ]

chunkedRequest :: Client.Request -> [Header] -> [BS.ByteString] -> Client.Request
chunkedRequest initial headers chunks =
    initial
        { Client.method = methodPost
        , Client.requestBody = Client.RequestBodyStreamChunked supply
        , Client.requestHeaders = headers
        , Client.decompress = const False
        }
  where
    supply popper = do
        source <- newIORef chunks
        popper $ do
            remaining <- readIORef source
            case remaining of
                [] -> pure BS.empty
                chunk : rest -> writeIORef source rest >> pure chunk

rejectedChunked :: BodyLimits -> [Header] -> LBS.ByteString -> Status -> Assertion
rejectedChunked limits headers body expected = do
    called <- newIORef False
    let application request respond = writeIORef called True >> echoApplication request respond
    testWithApplication (pure (decompressRequests limits codecs application)) $ \port -> do
        manager <- Client.newManager Client.defaultManagerSettings
        initial <- Client.parseRequest ("http://127.0.0.1:" <> show port <> "/echo")
        response <- Client.httpLbs (chunkedRequest initial headers (LBS.toChunks body)) manager
        Client.responseStatus response @?= expected
        readIORef called >>= (@?= False)

networkTests :: TestTree
networkTests =
    testGroup
        "real HTTP via Warp"
        [ testCase (BSC.unpack (codecName codec)) $
            testWithApplication (pure (compression settings codecs echoApplication)) $ \port -> do
                manager <- Client.newManager Client.defaultManagerSettings
                baseRequest <- Client.parseRequest ("http://127.0.0.1:" <> show port <> "/echo")
                encoded <- encodeBody codec payload
                let request =
                        baseRequest
                            { Client.method = methodPost
                            , Client.requestBody = Client.RequestBodyLBS encoded
                            , Client.requestHeaders = [(hContentEncoding, codecName codec), (hAcceptEncoding, codecName codec)]
                            , Client.decompress = const False
                            }
                response <- Client.httpLbs request manager
                Client.responseStatus response @?= status200
                lookup hContentEncoding (Client.responseHeaders response) @?= Just (codecName codec)
                decodeBody defaultBodyLimits codec (Client.responseBody response) >>= (@?= payload)
        | codec <- codecs
        ]

assertLeft :: (Show value) => Either error value -> Assertion
assertLeft result = case result of
    Left _ -> pure ()
    Right value -> assertFailure ("expected Left, got " <> show value)

expectInvalid :: IO LBS.ByteString -> Assertion
expectInvalid action = do
    result <- try @CompressionError action
    case result of
        Left (InvalidCompressedBody _) -> pure ()
        _ -> assertFailure ("expected invalid compressed body, got " <> show result)

withFile :: LBS.ByteString -> (FilePath -> IO result) -> IO result
withFile body action = do
    temporary <- getTemporaryDirectory
    bracket
        (openBinaryTempFile temporary "wai-compression-test")
        (\(path, _) -> removeFile path)
        (\(path, handle) -> LBS.hPut handle body >> hClose handle >> action path)

safetyTests :: TestTree
safetyTests =
    testGroup "additional safety boundaries" $
        [ testCase "encoding quality API" $ do
            encodingQuality "GZIP" [(hAcceptEncoding, "gzip;q=0.123")] @?= Right 123
            encodingQuality "identity" [] @?= Right 1000
            encodingQuality "gzip" [(hAcceptEncoding, "br")] @?= Right 0
        , testCase "decoder deadline" $ do
            codec <- either throwIO pure (mkCodec "slow" (\_ _ _ -> pure ()) (\_ _ -> threadDelay 100000))
            try @CompressionError (decodeBody (defaultBodyLimits{bodyTimeoutMicros = 1000}) codec "x") >>= (@?= Left BodyTimeout)
        , testCase "decoder must consume input" $ do
            codec <- either throwIO pure (mkCodec "non-consuming" (\_ _ _ -> pure ()) (\_ _ -> pure ()))
            expectInvalid (decodeBody defaultBodyLimits codec "x")
        , testCase "compressed input length check is short-circuiting" $ do
            let body = LBS.fromChunks ("too large" : error "input tail must not be evaluated")
            try @CompressionError (decodeBody (defaultBodyLimits{maxEncodedBytes = 1}) gzip body) >>= (@?= Left EncodedBodyTooLarge)
        , testCase "streaming encoder output quota" $
            try @CompressionError (encodeBodyBounded 1 gzip payload) >>= (@?= Left DecodedBodyTooLarge)
        , testCase "high Zstandard levels keep a decodable HTTP-sized window" $
            forM_ [20, 22] $ \level -> do
                codec <- either throwIO pure (zstdWith level)
                let body = payload <> payload
                encoded <- encodeBody codec body
                let header = LBS.toStrict (LBS.take 6 encoded)
                assertBool "Zstandard window exceeds 8 MiB" (BS.length header == 6 && BS.index header 5 <= 104)
                decodeBody defaultBodyLimits codec encoded >>= (@?= body)
        , testCase "buffered codec rejects invalid quota" $ assertLeft (mkBufferedCodec 0 "reverse" BS.reverse BS.reverse)
        , testCase "decoded length must fit WAI Word64" $
            assertLeft (validateBodyLimits (defaultBodyLimits{maxDecodedBytes = 2 ^ (64 :: Int)}))
        , testCase "oversized Content-Encoding field" $ do
            response <- upload defaultBodyLimits [(hContentEncoding, BS.replicate 8193 97)] "opaque"
            WaiTest.simpleStatus response @?= status400
        , testCase "empty Content-Encoding alongside a real coding" $ do
            response <- upload defaultBodyLimits [(hContentEncoding, ""), (hContentEncoding, "gzip")] "opaque"
            WaiTest.simpleStatus response @?= status400
        , testCase "cumulative multi-layer expansion quota" $ do
            encoded <- encodeBody gzip payload >>= encodeBody brotli
            response <- upload (defaultBodyLimits{maxExpansionRatio = 5, expansionRatioSlack = 0}) [(hContentEncoding, "gzip,br")] encoded
            WaiTest.simpleStatus response @?= status413
        , testCase "invalid response threshold" $ do
            result <- try @CompressionError (getResponse (compressResponses (responseOptions{responseMinimumSize = -1}) [gzip]) defaultRequest plainResponse)
            case result of
                Left (InvalidConfiguration _) -> pure ()
                _ -> assertFailure "negative threshold must be a configuration error"
        , testCase "sensitive opt-in is explicit" $ do
            response <-
                getResponse
                    (compressResponses (responseOptions{responseAllowSensitive = True}) [gzip])
                    (requestWith [(hCookie, "session=secret"), (hAcceptEncoding, "gzip")])
                    plainResponse
            lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Just "gzip"
        , testCase "missing content type is not guessed" $ do
            response <- getResponse (compressResponses responseOptions [gzip]) (requestWith [(hAcceptEncoding, "gzip")]) (responseLBS status200 [] payload)
            lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Nothing
        , testCase "known small stream is not compressed" $ do
            response <-
                getResponse
                    (compressResponses responseOptions [gzip])
                    (requestWith [(hAcceptEncoding, "gzip")])
                    (responseStream status200 [(hContentType, "text/plain"), (hContentLength, "1")] (\send _ -> send (Builder.byteString "x")))
            lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Nothing
        , testCase "file slices are never compressed" $ withFile payload $ \path -> do
            response <-
                getResponse
                    (compressResponses (responseOptions{responseCompressFiles = True}) [gzip])
                    (requestWith [(hAcceptEncoding, "gzip")])
                    (responseFile status200 [(hContentType, "text/plain")] path (Just (FilePart 0 2000 (fromIntegral (LBS.length payload)))))
            lookup hContentEncoding (WaiTest.simpleHeaders response) @?= Nothing
            LBS.length (WaiTest.simpleBody response) @?= 2000
        , testCase "upstream identity passthrough" $ do
            response <- getResponse (decompressResponses defaultBodyLimits codecs) defaultRequest plainResponse
            WaiTest.simpleBody response @?= payload
        , testCase "upstream raw passthrough" $ do
            response <- getResponse (decompressResponses defaultBodyLimits codecs) defaultRequest (responseRaw (\_ _ -> pure ()) plainResponse)
            WaiTest.simpleBody response @?= payload
        , testCase "upstream decoder quota is checked before emitting" $ do
            encoded <- encodeBody gzip payload
            response <-
                getResponse
                    (decompressResponses (defaultBodyLimits{maxEncodedBytes = 1}) codecs)
                    defaultRequest
                    (responseLBS status200 [(hContentEncoding, "gzip")] encoded)
            WaiTest.simpleStatus response @?= status502
        , testCase "proxy does not double-encode" $ do
            encoded <- encodeBody gzip payload
            response <-
                WaiTest.runSession
                    (WaiTest.srequest (WaiTest.SRequest (requestWith [(hContentEncoding, "gzip")]) encoded))
                    (compressRequests defaultBodyLimits brotli echoApplication)
            WaiTest.simpleBody response @?= encoded
        , testCase "proxy respects request no-transform" $ do
            response <-
                WaiTest.runSession
                    (WaiTest.srequest (WaiTest.SRequest (requestWith [(hCacheControl, "no-transform")]) payload))
                    (compressRequests defaultBodyLimits gzip echoApplication)
            WaiTest.simpleBody response @?= payload
        , testCase "proxy input quota returns 413" $ do
            response <-
                WaiTest.runSession
                    (WaiTest.srequest (WaiTest.SRequest defaultRequest payload))
                    (compressRequests (defaultBodyLimits{maxDecodedBytes = 1}) gzip echoApplication)
            WaiTest.simpleStatus response @?= status413
        , testCase "proxy quotas distinguish plain and encoded lengths" $ do
            encoded <- encodeBody gzip payload
            let limits =
                    defaultBodyLimits
                        { maxEncodedBytes = toInteger (LBS.length encoded)
                        , maxDecodedBytes = toInteger (LBS.length payload)
                        }
            response <-
                WaiTest.runSession
                    (WaiTest.srequest (WaiTest.SRequest defaultRequest payload))
                    (compressRequests limits gzip echoApplication)
            WaiTest.simpleStatus response @?= status200
            decodeBody defaultBodyLimits gzip (WaiTest.simpleBody response) >>= (@?= payload)
        , testCase "proxy output quota returns 413" $ do
            response <-
                WaiTest.runSession
                    (WaiTest.srequest (WaiTest.SRequest defaultRequest payload))
                    (compressRequests (defaultBodyLimits{maxEncodedBytes = 1}) gzip echoApplication)
            WaiTest.simpleStatus response @?= status413
        , testCase "gzip checksum failure is rejected" $ do
            encoded <- encodeBody gzip payload
            let position = LBS.length encoded - 8
                corrupted = LBS.take position encoded <> LBS.singleton (LBS.index encoded position `xor` 1) <> LBS.drop (position + 1) encoded
            expectInvalid (decodeBody defaultBodyLimits gzip corrupted)
        , testCase "deflate checksum failure is rejected" $ do
            encoded <- encodeBody deflate payload
            let position = LBS.length encoded - 1
                corrupted = LBS.take position encoded <> LBS.singleton (LBS.index encoded position `xor` 1)
            expectInvalid (decodeBody defaultBodyLimits deflate corrupted)
        , testCase "all input chunks can be single bytes" $ do
            encoded <- encodeBody gzip payload
            source <- newIORef (map BS.singleton (LBS.unpack encoded))
            captured <- newIORef LBS.empty
            let pull = do
                    chunks <- readIORef source
                    case chunks of
                        [] -> pure BS.empty
                        chunk : remaining -> writeIORef source remaining >> pure chunk
                request = setRequestBodyChunks pull (requestWith [(hContentEncoding, "gzip")])
            void
                ( decompressRequests
                    defaultBodyLimits
                    codecs
                    echoApplication
                    request
                    ( \response -> do
                        let (_, _, withBody) = responseToStream response
                        withBody (\body -> body (\builder -> modifyIORef' captured (<> Builder.toLazyByteString builder)) (pure ()))
                        pure ResponseReceived
                    )
                )
            readIORef captured >>= (@?= payload)
        ]
            <> [ testCase ("non-transformable upstream " <> description) $ do
                    encoded <- encodeBody gzip payload
                    response <-
                        getResponse
                            (decompressResponses defaultBodyLimits codecs)
                            defaultRequest
                            (responseLBS responseCode ((hContentEncoding, "gzip") : headers) encoded)
                    WaiTest.simpleStatus response @?= status502
               | (description, responseCode, headers) <-
                    [ ("no-transform", status200, [(hCacheControl, "no-transform")])
                    , ("partial", status206, [])
                    , ("Content-Range", status200, [(hContentRange, "bytes 0-10/20")])
                    ]
               ]
            <> [ testCase ("large incompressible roundtrip " <> BSC.unpack (codecName codec)) $ do
                    let randomBytes = LBS.pack (take 150000 (map (fromIntegral . (`shiftR` 24)) (drop 1 (iterate (\seed -> seed * 1664525 + 1013904223) (123456 :: Word32)))))
                    encoded <- encodeBody codec randomBytes
                    assertBool "must exercise large encoded output" (LBS.length encoded > 32768)
                    decodeBody defaultBodyLimits codec encoded >>= (@?= randomBytes)
               | codec <- codecs
               ]

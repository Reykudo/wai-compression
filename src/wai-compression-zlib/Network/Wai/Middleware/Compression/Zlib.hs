{-# LANGUAGE OverloadedStrings #-}

{- | Optional streaming gzip and zlib-wrapped HTTP deflate codecs. Profiles map
to levels 1 (Fast), 6 (Balanced) and 9 (Compact); flushes are preserved.
-}
module Network.Wai.Middleware.Compression.Zlib (
    gzip,
    gzipWith,
    deflate,
    deflateWith,
) where

import Codec.Compression.Zlib.Internal qualified as Zlib
import Control.Exception (throw, throwIO)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Streaming.Zlib qualified as Streaming
import Network.Wai.Middleware.Compression.Codec
import Network.Wai.Middleware.Compression.Encoding qualified as Encoding

{- | Adaptive @gzip@ codec. Complete input and checksums are validated; decoding
supports concatenated gzip members.
-}
gzip :: Codec
gzip = adaptiveZlib Encoding.gzip gzipWith

-- | Fixed gzip level from 0 to 9, unaffected by encoding-policy profiles.
gzipWith :: Int -> Either CompressionError Codec
gzipWith = makeZlib Encoding.gzip Zlib.gzipFormat 31

-- | Adaptive HTTP @deflate@ codec using the zlib wrapper, not raw DEFLATE.
deflate :: Codec
deflate = adaptiveZlib Encoding.deflate deflateWith

-- | Fixed HTTP deflate level from 0 to 9, unaffected by policy profiles.
deflateWith :: Int -> Either CompressionError Codec
deflateWith = makeZlib Encoding.deflate Zlib.zlibFormat 15

adaptiveZlib :: BS.ByteString -> (Int -> Either CompressionError Codec) -> Codec
adaptiveZlib name make = either throw id $ do
    fast <- make 1
    balanced <- make 6
    compact <- make 9
    let encoder profile = codecEncode $ case profile of
            Fast -> fast
            Balanced -> balanced
            Compact -> compact
    mkAdaptiveCodec name encoder (codecDecode balanced)

makeZlib :: BS.ByteString -> Zlib.Format -> Int -> Int -> Either CompressionError Codec
makeZlib name format window level
    | level < 0 || level > 9 = Left (InvalidConfiguration "zlib compression level must be between 0 and 9")
    | otherwise = mkCodec name encode decode
  where
    encode :: Encoder
    encode producer emit flush = do
        context <- Streaming.initDeflate level (Streaming.WindowBits window)
        let drain popper = do
                result <- popper
                case result of
                    Streaming.PRDone -> pure ()
                    Streaming.PRNext chunk -> unless (BS.null chunk) (emit chunk) >> drain popper
                    Streaming.PRError failure -> throwIO failure
        producer
            (\chunk -> unless (BS.null chunk) (Streaming.feedDeflate context chunk >>= drain))
            (drain (Streaming.flushDeflate context) >> flush)
        drain (Streaming.finishDeflate context)
    decode :: Decoder
    decode pull emit = loop (Zlib.decompressIO format Zlib.defaultDecompressParams)
      where
        loop (Zlib.DecompressInputRequired supply) = pull >>= supply >>= loop
        loop (Zlib.DecompressOutputAvailable chunk next) = unless (BS.null chunk) (emit chunk) >> next >>= loop
        loop (Zlib.DecompressStreamEnd unused) = do
            unless (BS.null unused) invalidTrailing
            extra <- pull
            unless (BS.null extra) invalidTrailing
        loop (Zlib.DecompressStreamError failure) = throwIO (InvalidCompressedBody (show failure))
        invalidTrailing = throwIO (InvalidCompressedBody "trailing zlib data")

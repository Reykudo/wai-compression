{-# LANGUAGE OverloadedStrings #-}

{- | Optional streaming Brotli codec for the @br@ content coding. Profiles map
to levels 3 (Fast), 5 (Balanced) and 6 (Compact); flushes are preserved.
-}
module Network.Wai.Middleware.Compression.Brotli (
    brotli,
    brotliWith,
) where

import Codec.Compression.Brotli qualified as Brotli
import Control.Exception (catch, throw, throwIO)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.Wai.Middleware.Compression.Codec
import Network.Wai.Middleware.Compression.Encoding qualified as Encoding

-- | Adaptive Brotli codec. Decoding rejects truncation and trailing bytes.
brotli :: Codec
brotli = either throw id $ do
    fast <- brotliWith 3
    balanced <- brotliWith 5
    compact <- brotliWith 6
    let encoder profile = codecEncode $ case profile of
            Fast -> fast
            Balanced -> balanced
            Compact -> compact
    mkAdaptiveCodec Encoding.brotli encoder (codecDecode balanced)

-- | Fixed Brotli quality from 0 to 11, unaffected by encoding-policy profiles.
brotliWith :: Int -> Either CompressionError Codec
brotliWith level
    | level < 0 || level > 11 = Left (InvalidConfiguration "Brotli compression level must be between 0 and 11")
    | otherwise = mkCodec Encoding.brotli encode decode
  where
    parameters = Brotli.defaultCompressParams{Brotli.compressLevel = toEnum level}
    encode :: Encoder
    encode producer emit flush = do
        initial <- Brotli.compressIO parameters
        state <- newIORef initial
        let drain (Brotli.CompressOutputAvailable chunk next) = unless (BS.null chunk) (emit chunk) >> next >>= drain
            drain next = writeIORef state next
            feed chunk = unless (BS.null chunk) $ do
                current <- readIORef state
                case current of
                    Brotli.CompressInputRequired _ supply -> supply chunk >>= drain
                    _ -> throwIO (InvalidConfiguration "Brotli encoder is not ready")
            flushEncoder = do
                current <- readIORef state
                case current of
                    Brotli.CompressInputRequired flushCodec _ -> flushCodec >>= drain
                    _ -> throwIO (InvalidConfiguration "Brotli encoder is not ready")
                flush
        producer feed flushEncoder
        current <- readIORef state
        case current of
            Brotli.CompressInputRequired _ supply -> supply BS.empty >>= drain
            _ -> throwIO (InvalidConfiguration "Brotli encoder is not ready")
        final <- readIORef state
        case final of
            Brotli.CompressStreamEnd -> pure ()
            _ -> throwIO (InvalidConfiguration "Brotli encoder did not finish")
    decode :: Decoder
    decode pull emit =
        (loop =<< Brotli.decompressIO Brotli.defaultDecompressParams)
            `catch` (\(failure :: Brotli.BrotliException) -> throwIO (InvalidCompressedBody (show failure)))
      where
        loop (Brotli.DecompressInputRequired supply) = pull >>= supply >>= loop
        loop (Brotli.DecompressOutputAvailable chunk next) = unless (BS.null chunk) (emit chunk) >> next >>= loop
        loop (Brotli.DecompressStreamEnd unused) = do
            unless (BS.null unused) invalidTrailing
            extra <- pull
            unless (BS.null extra) invalidTrailing
        loop (Brotli.DecompressStreamError failure) = throwIO (InvalidCompressedBody (show failure))
        invalidTrailing = throwIO (InvalidCompressedBody "trailing Brotli data")

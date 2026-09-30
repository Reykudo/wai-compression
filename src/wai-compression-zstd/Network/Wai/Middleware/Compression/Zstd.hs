{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Optional streaming Zstandard codec. Profiles map to levels 1 (Fast),
3 (Balanced) and 6 (Compact). The decoder's advertised window limit is 8 MiB.
-}
module Network.Wai.Middleware.Compression.Zstd (
    zstd,
    zstdWith,
) where

import Codec.Compression.Zstd.FFI qualified as Zstd
import Control.Exception (bracket, throw, throwIO)
import Control.Monad (unless, void, when)
import Data.ByteString qualified as BS
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import Network.Wai.Middleware.Compression.Codec
import Network.Wai.Middleware.Compression.Encoding qualified as Encoding

foreign import ccall unsafe "ZSTD_DCtx_setParameter"
    setDecoderParameter :: Ptr Zstd.DStream -> CInt -> CInt -> IO CSize

foreign import ccall unsafe "ZSTD_CCtx_setParameter"
    setEncoderParameter :: Ptr Zstd.CStream -> CInt -> CInt -> IO CSize

{- | Adaptive @zstd@ codec with complete frame validation, concatenated-frame
support and preserved flush boundaries.
-}
zstd :: Codec
zstd = either throw id $ do
    fast <- zstdWith 1
    balanced <- zstdWith 3
    compact <- zstdWith 6
    let encoder profile = codecEncode $ case profile of
            Fast -> fast
            Balanced -> balanced
            Compact -> compact
    mkAdaptiveCodec Encoding.zstd encoder (codecDecode balanced)

{- | Fixed native compression level from 1 to the library's supported maximum.
Levels 20 and above cap the encoder window at 8 MiB for decoder compatibility.
Profiles do not override the chosen level; other native allocations are not
bounded by the window limit alone.
-}
zstdWith :: Int -> Either CompressionError Codec
zstdWith level
    | level < 1 || level > Zstd.maxCLevel = Left (InvalidConfiguration "unsupported Zstandard compression level")
    | otherwise = mkCodec Encoding.zstd encode decode
  where
    encode :: Encoder
    encode producer emit flush = bracket (allocate Zstd.createCStream) (void . Zstd.freeCStream) $ \context -> do
        void (checkEncoding =<< Zstd.initCStream context (fromIntegral level))
        when (level >= 20) (void (checkEncoding =<< setEncoderParameter context 101 23))
        allocaBytes bufferSize $ \output -> alloca $ \outputBuffer -> do
            let drain action = do
                    poke outputBuffer (Zstd.Buffer output (fromIntegral bufferSize) 0)
                    remaining <- checkEncoding =<< action context outputBuffer
                    produced <- Zstd.bufPos <$> peek outputBuffer
                    sendOutput output produced emit
                    when (remaining /= 0) (drain action)
                feed chunk = unless (BS.null chunk) $ BS.useAsCStringLen chunk $ \(input, size) -> alloca $ \inputBuffer -> do
                    poke inputBuffer (Zstd.Buffer (castPtr input) (fromIntegral size) 0)
                    let loop = do
                            before <- Zstd.bufPos <$> peek inputBuffer
                            poke outputBuffer (Zstd.Buffer output (fromIntegral bufferSize) 0)
                            void (checkEncoding =<< Zstd.compressStream context outputBuffer inputBuffer)
                            consumed <- Zstd.bufPos <$> peek inputBuffer
                            produced <- Zstd.bufPos <$> peek outputBuffer
                            sendOutput output produced emit
                            when (consumed < fromIntegral size) $ do
                                when (consumed == before && produced == 0) (throwIO (InvalidConfiguration "Zstandard encoder made no progress"))
                                loop
                    loop
            producer feed (drain Zstd.flushStream >> flush)
            drain Zstd.endStream
    decode :: Decoder
    decode pull emit = bracket (allocate Zstd.createDStream) (void . Zstd.freeDStream) $ \context -> do
        void (check =<< Zstd.initDStream context)
        void (check =<< setDecoderParameter context 100 23)
        allocaBytes bufferSize $ \output -> alloca $ \outputBuffer -> do
            let process chunk seenFrame = BS.useAsCStringLen chunk $ \(input, size) -> alloca $ \inputBuffer -> do
                    poke inputBuffer (Zstd.Buffer (castPtr input) (fromIntegral size) 0)
                    let loop frameSeen = do
                            before <- Zstd.bufPos <$> peek inputBuffer
                            poke outputBuffer (Zstd.Buffer output (fromIntegral bufferSize) 0)
                            remaining <- check =<< Zstd.decompressStream context outputBuffer inputBuffer
                            consumed <- Zstd.bufPos <$> peek inputBuffer
                            produced <- Zstd.bufPos <$> peek outputBuffer
                            sendOutput output produced emit
                            let completed = remaining == 0
                                seen = frameSeen || completed
                            if consumed < fromIntegral size || (produced == fromIntegral bufferSize && not completed)
                                then do
                                    when (consumed == before && produced == 0) (throwIO (InvalidCompressedBody "Zstandard decoder made no progress"))
                                    loop seen
                                else pure (completed, seen)
                    loop seenFrame
                next atBoundary seenFrame = do
                    chunk <- pull
                    if BS.null chunk
                        then
                            unless (atBoundary && seenFrame) (throwIO (InvalidCompressedBody "truncated Zstandard frame"))
                        else do
                            (boundary, seen) <- process chunk seenFrame
                            next boundary seen
            next False False

bufferSize :: Int
bufferSize = 32768

allocate :: IO (Ptr context) -> IO (Ptr context)
allocate action = do
    context <- action
    when (context == nullPtr) (throwIO (InvalidConfiguration "could not allocate Zstandard context"))
    pure context

check :: CSize -> IO CSize
check result
    | Zstd.isError result = throwIO (InvalidCompressedBody (Zstd.getErrorName result))
    | otherwise = pure result

checkEncoding :: CSize -> IO CSize
checkEncoding result
    | Zstd.isError result = throwIO (InvalidConfiguration (Zstd.getErrorName result))
    | otherwise = pure result

sendOutput :: Ptr buffer -> CSize -> (BS.ByteString -> IO ()) -> IO ()
sendOutput pointer size emit = when (size /= 0) (BS.packCStringLen (castPtr pointer, fromIntegral size) >>= emit)

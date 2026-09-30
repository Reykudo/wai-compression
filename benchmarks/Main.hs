module Main (main) where

import Control.Exception (evaluate, throwIO)
import Control.Monad (forM_, replicateM, replicateM_)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (intercalate, nub, sort)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Wai.Middleware.Compression.Brotli (brotliWith)
import Network.Wai.Middleware.Compression.Codec
import Network.Wai.Middleware.Compression.Zlib (deflateWith, gzipWith)
import Network.Wai.Middleware.Compression.Zstd (zstdWith)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Mem (performGC)
import Text.Printf (printf)

data Candidate = Candidate String Int Codec

main :: IO ()
main = do
    arguments <- getArgs
    directory <- case arguments of
        [path] -> pure path
        _ -> fail "Usage: wai-compression-benchmarks CORPUS_DIRECTORY"
    candidates <-
        sequence $
            [candidate "gzip" level (gzipWith level) | level <- [1, 6, 9]]
                <> [candidate "deflate" level (deflateWith level) | level <- [1, 6, 9]]
                <> [candidate "br" level (brotliWith level) | level <- [3, 4, 5, 6, 9]]
                <> [candidate "zstd" level (zstdWith level) | level <- [1, 3, 6, 9]]
    putStrLn "dataset,bytes,encoding,level,compressed_bytes,ratio,encode_us,encode_min_us,encode_max_us,decode_us"
    forM_ ["data.json", "page.html", "text-en.txt", "text-ru.txt"] $ \filename -> do
        input <- BS.readFile (directory </> filename)
        if BS.length input < 65536 then fail (filename <> " must contain at least 64 KiB") else pure ()
        let sizes = nub (map (min (BS.length input)) [512, 1024, 4096, 16384, 65536, 262144, 1048576])
        forM_ sizes $ \size -> do
            hPutStrLn stderr (filename <> ": " <> show size <> " bytes")
            let body = LBS.fromStrict (BS.take size input)
            forM_ candidates $ \(Candidate name level codec) -> do
                encoded <- encodeBody codec body
                compressedSize <- evaluate (LBS.length encoded)
                let limits = defaultBodyLimits{maxExpansionRatio = 1000}
                decoded <- decodeBody limits codec encoded
                if decoded /= body then fail ("Roundtrip failed: " <> name) else pure ()
                encoding <- measure (encodeBody codec body)
                decoding <- measure (decodeBody limits codec encoded)
                putStrLn $
                    intercalate
                        ","
                        [ filename
                        , show size
                        , name
                        , show level
                        , show compressedSize
                        , printf "%.6f" (fromIntegral compressedSize / fromIntegral size :: Double)
                        , printf "%.3f" (median encoding)
                        , printf "%.3f" (minimum encoding)
                        , printf "%.3f" (maximum encoding)
                        , printf "%.3f" (median decoding)
                        ]

candidate :: String -> Int -> Either CompressionError Codec -> IO Candidate
candidate name level codec = Candidate name level <$> either throwIO pure codec

measure :: IO LBS.ByteString -> IO [Double]
measure action = do
    warmup <- elapsed 1 action
    let iterations = max 1 (min 256 (ceiling (10000 / max 1 warmup)))
    replicateM 5 $ do
        performGC
        elapsed iterations action

elapsed :: Int -> IO LBS.ByteString -> IO Double
elapsed iterations action = do
    started <- getMonotonicTimeNSec
    replicateM_ iterations (action >>= evaluate . LBS.length)
    finished <- getMonotonicTimeNSec
    pure (fromIntegral (finished - started) / (1000 * fromIntegral iterations))

median :: [Double] -> Double
median values = sort values !! (length values `div` 2)

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Compression
import Network.Wai.Middleware.Compression.Brotli (brotli)
import Network.Wai.Middleware.Compression.Zlib (deflate, gzip)
import Network.Wai.Middleware.Compression.Zstd (zstd)

main :: IO ()
main = do
    putStrLn "Listening on http://127.0.0.1:8080 (GET /public, POST /echo)"
    run 8080 (compression settings [brotli, zstd, gzip, deflate] application)
  where
    settings =
        defaultCompressionSettings
            { responseSettings =
                defaultResponseSettings
                    { responsePolicy = \request _ -> pathInfo request == ["public"]
                    , responseEncodingPolicy = recommendedPolicy
                    }
            }

application :: Application
application request respond = case (requestMethod request, pathInfo request) of
    ("GET", ["public"]) ->
        respond
            ( responseLBS
                status200
                [(hContentType, "text/plain; charset=utf-8"), (hCacheControl, "public, max-age=60")]
                (LBS.fromChunks (replicate 1024 "Public information. No cookies or secrets.\n"))
            )
    ("POST", ["echo"]) -> do
        body <- strictRequestBody request
        respond (responseLBS status200 [(hContentType, "application/octet-stream")] body)
    _ -> respond (responseLBS status404 [(hContentType, "text/plain")] "Not found")

{-# LANGUAGE OverloadedStrings #-}

{- | HTTP content-coding names without codec dependencies. Import qualified to
avoid collisions with adapter values, which have type @Codec@.
-}
module Network.Wai.Middleware.Compression.Encoding (
    gzip,
    deflate,
    brotli,
    zstd,
) where

import Data.ByteString (ByteString)

-- | The @gzip@ wire name.
gzip :: ByteString
gzip = "gzip"

-- | The @deflate@ wire name, representing zlib-wrapped HTTP deflate.
deflate :: ByteString
deflate = "deflate"

-- | The Brotli wire name, @br@.
brotli :: ByteString
brotli = "br"

-- | The @zstd@ wire name.
zstd :: ByteString
zstd = "zstd"

# wai-compression-zstd

Optional streaming Zstandard adapter for `wai-compression`.
Import `Network.Wai.Middleware.Compression.Zstd` and register `zstd`.
`zstdWith` selects an explicit compression level supported by the native library.

Adaptive profiles use levels 1 (Fast), 3 (Balanced) and 6 (Compact). Explicit
levels are never overridden by the response encoding policy. The decoder limits
the advertised window to 8 MiB; encoder levels 20 and above use the same bound.
Concatenated frames are supported. Incomplete input and trailing junk are rejected.

Author: https://github.com/Reykudo. License: MIT; see `LICENSE`.

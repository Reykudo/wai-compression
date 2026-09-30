# wai-compression-zlib

Optional streaming gzip and HTTP deflate adapters for `wai-compression`.
Import `Network.Wai.Middleware.Compression.Zlib` and register `gzip` or `deflate`.
`gzipWith` and `deflateWith` select explicit compression levels from 0 to 9.

Adaptive profiles use levels 1 (Fast), 6 (Balanced) and 9 (Compact). Explicit
levels are never overridden by the response encoding policy. HTTP deflate uses
the zlib wrapper, not raw DEFLATE. Decoders validate checksums and complete input;
gzip concatenated members are supported.

Author: https://github.com/Reykudo. License: MIT; see `LICENSE`.

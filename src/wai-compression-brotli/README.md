# wai-compression-brotli

Optional streaming Brotli adapter for `wai-compression`.
Import `Network.Wai.Middleware.Compression.Brotli` and register `brotli` for `br`.
`brotliWith` selects an explicit compression level from 0 to 11.

Adaptive profiles use levels 3 (Fast), 5 (Balanced) and 6 (Compact). Explicit
levels are never overridden by the response encoding policy. Streaming responses
preserve flushes; request decoding rejects incomplete input and trailing bytes.

Author: https://github.com/Reykudo. License: MIT; see `LICENSE`.

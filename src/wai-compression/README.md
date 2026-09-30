# wai-compression

Codec-independent WAI middleware for negotiated response compression and bounded
request decoding. The core does not depend on zlib, Brotli or Zstandard.

Use `Network.Wai.Middleware.Compression` for settings and middleware,
`Network.Wai.Middleware.Compression.Codec` for custom streaming codecs,
`Network.Wai.Middleware.Compression.Encoding` for wire-name constants, and
`Network.Wai.Middleware.Compression.Policy` for the optional recommended policy.

Response compression is disabled by default. Set `responsePolicy` explicitly for
responses that are safe to compress. Request decoding validates complete bodies
within configurable byte, expansion, layer and time limits before the application
runs. Chunked uploads are supported; decoded requests are bounded but buffered.
Responses stream and preserve flushes. Client `Accept-Encoding` quality values
take precedence over the server's policy.

Optional adapters are separate packages: `wai-compression-zlib`,
`wai-compression-brotli` and `wai-compression-zstd`. Applications can register only
the adapters they need, or provide their own through `mkCodec`.

Author: https://github.com/Reykudo. License: MIT; see `LICENSE`.

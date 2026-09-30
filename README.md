# wai-compression

Codec-independent WAI middleware for request and response body transformations.
The core package does **not** depend on `zlib`, `brotli`, `zstd`, or any native
compression library. Applications supply the codecs they want to support, in
server-preference order. Optional adapter packages demonstrate safe incremental
integration with the corresponding Haskell libraries.

## Layout

```text
cabal.project
flake.nix
src/
  wai-compression/          # Core middleware, policy, negotiation, codec interface
  wai-compression-zlib/     # Optional gzip and HTTP deflate
  wai-compression-brotli/   # Optional br
  wai-compression-zstd/     # Optional zstd
tests/                     # Separate test package
example/                   # Separate executable package
benchmarks/                # Corpus runner and recorded policy measurements
```

There is no root `.cabal` file. Consumers depend only on `wai-compression` and
the adapter packages they actually use; the workspace's tests and example use
all adapters but are not transitive dependencies of the library. A gzip-only
application does not acquire Brotli or Zstandard dependencies.

## Using the middleware

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Middleware.Compression
import Network.Wai.Middleware.Compression.Brotli (brotli)
import Network.Wai.Middleware.Compression.Zlib (gzip)
import Network.Wai.Middleware.Compression.Zstd (zstd)

middleware :: Middleware
middleware = compression options [brotli, zstd, gzip]
  where
    options = defaultCompressionSettings
      { responseSettings = defaultResponseSettings
          { responsePolicy = \request _ -> pathInfo request == ["public"] }
      }
```

`compression` decodes compressed incoming requests **before** calling the
application and negotiates response compression afterwards. Response
compression is deliberately **disabled by default**: the application must
explicitly identify responses safe to compress. Body decompression is enabled
for the registered codecs, subject to limits.

Without an encoding policy, the default codecs use levels gzip/deflate 6,
Brotli 5, and Zstandard 3. Their
`*With` constructors validate custom levels and return `Either CompressionError
Codec`. The `deflate` adapter uses the HTTP zlib-wrapped format, not raw DEFLATE.

### Content-coding names

The core exports standard content-coding names as strict `ByteString` constants:

```haskell
import Data.ByteString (ByteString)
import Network.Wai.Middleware.Compression.Encoding qualified as Encoding

preferredEncodings :: [ByteString]
preferredEncodings = [Encoding.brotli, Encoding.zstd, Encoding.gzip]
```

`Encoding.gzip`, `Encoding.deflate`, `Encoding.brotli`, and `Encoding.zstd` contain
`"gzip"`, `"deflate"`, `"br"`, and `"zstd"`, respectively. Use them with `mkCodec`,
`mkBufferedCodec`, or `negotiateEncoding` instead of spelling HTTP tokens manually.
They do not register codecs or introduce compression-library dependencies, and
custom names remain supported. The constants have their own public module to
avoid collisions with the adapters' `Codec` values of the same names.

### Optional recommended policy

Enable the heuristic independently of the application's security policy:

```haskell
options = defaultCompressionSettings
  { responseSettings = defaultResponseSettings
      { responsePolicy = \request _ -> pathInfo request == ["public"]
      , responseEncodingPolicy = recommendedPolicy
      }
  }
```

`responsePolicy` still decides whether compression is safe. `responseEncodingPolicy`
only reorders registered codec names and selects a compression profile. Client
`q` values remain higher priority; `q=0` is never overridden. Missing `q` means
1. With the default policy, registration order breaks ties and levels remain
unchanged. A custom `EncodingPolicy` must return each registered name exactly
once; unknown, missing or duplicate names are configuration errors.

The optional recommendation uses the normalized MIME type and the uncompressed
`Content-Length` hint, never the full body length:

| Metadata | Preference | Profile |
| --- | --- | --- |
| Unknown size or less than 16 KiB, text/JSON/markup | zstd, br, gzip, deflate | Fast |
| At least 16 KiB, HTML/CSS/JavaScript/XML/SVG | br, zstd, gzip, deflate | Balanced |
| At least 16 KiB, JSON or other text | zstd, br, gzip, deflate | Balanced |
| Other MIME types | Original registration order | Same size-based profile |

Unavailable codecs are skipped; custom codecs follow the recognized codecs in
their original relative order. This is a latency/bandwidth compromise, not a
promise of the smallest result. It does not sniff language or body contents.
Only HTML, JSON, English text and Russian text were benchmarked directly; other
markup types are a replaceable extrapolation. See `benchmarks/README.md` for
the corpus, measurements, rationale and limitations.

Default adapters support the following profiles:

| Profile | gzip/deflate | Brotli | Zstandard |
| --- | --- | --- | --- |
| Fast | 1 | 3 | 1 |
| Balanced | 6 | 5 | 3 |
| Compact | 9 | 6 | 6 |

The recommendation uses Fast and Balanced only. A custom policy may request
Compact. Explicit `gzipWith`, `deflateWith`, `brotliWith` and `zstdWith` values
keep their requested level, regardless of the policy. `mkCodec` and
`mkBufferedCodec` also ignore profiles. Use `mkAdaptiveCodec` to supply a
profile-dependent encoder; `codecEncodeWith` selects a profile directly,
whereas `codecEncode` uses Balanced.

`responseInfo` reads headers only. An invalid, duplicate or longer-than-20-digit
`Content-Length` is an unknown size. If the application already knows the size
without inspecting a lazy body, it can supply that hint without changing headers:

```haskell
responseEncodingPolicy = \_ response available ->
  recommendedPolicyFor
    ((responseInfo response) { responseBodySize = Just knownSize })
    available
```

`knownSize` is the uncompressed byte count, not characters. It is an advisory
hint, not a replacement for decoding quotas or a framing guarantee. Never use
`LBS.length` just to populate it. Unknown-length streams select their encoder
once, before producing body chunks; no trial compression, full buffering or
mid-stream codec switching is performed by response compression.

### Your own encode/decode functions

For small, bounded bodies, no adapter package is required:

```haskell
myCodec :: Either CompressionError Codec
myCodec = mkBufferedCodec (1024 * 1024) "my-coding" myEncode myDecode

myEncode, myDecode :: ByteString -> ByteString
```

The arguments after the limit and wire token are **encode, then decode**.
`mkBufferedCodec` buffers a whole message, skips intermediate flushes, limits
input before calling your function, and checks output afterwards. This is a
convenience API, **not a decompression-bomb sandbox**: a strict
`ByteString -> ByteString` function necessarily allocates its result before the
core can check its size. It can also loop or allocate unbounded internal/native
memory. Invalid input handling remains the supplied function's responsibility;
prefer throwing `InvalidCompressedBody` over an untyped runtime exception.

For untrusted input and streaming responses, use `mkCodec`:

```haskell
type Producer = (ByteString -> IO ()) -> IO () -> IO ()
type Encoder = Producer -> (ByteString -> IO ()) -> IO () -> IO ()
type Decoder = IO ByteString -> (ByteString -> IO ()) -> IO ()

mkCodec :: ByteString -> Encoder -> Decoder -> Either CompressionError Codec
```

The encoder receives a producer, an output callback, and the downstream flush
action. It must emit a valid final frame, ignore empty input writes, and drain
compressed output before flushing downstream. Producer callbacks must be
sequential within an encoding operation; sharing a `Codec` across independent
concurrent requests is supported. The decoder pulls until an empty
chunk indicates EOF, emits bounded-sized chunks, validates end-of-frame and
checksums, rejects trailing garbage, and throws `CompressionError` on invalid
input. Codec tokens must be lowercase HTTP tokens other than `identity` and `*`.
Duplicate registrations are configuration errors.

Callbacks are trusted plugin code: call each continuation exactly once, do not
retain callbacks or use them concurrently, and let callback exceptions unwind
the codec. The middleware cannot constrain arbitrary code or arbitrary native
allocations. All supplied adapters use incremental APIs; Zstandard additionally
limits the decoder window to 8 MiB and brackets its native contexts.

### Separate operations

| Middleware | Purpose |
| --- | --- |
| `decompressRequests limits codecs` | Validate/decode incoming uploads before the application |
| `compressResponses settings codecs` | Negotiate and stream outgoing compression |
| `decompressResponses limits codecs` | Validate/buffer/decode compressed upstream responses |
| `compressRequests limits codec` | Buffer/encode a request for a downstream proxy application |

Request compression is **explicit**, not negotiated from request
`Accept-Encoding`, which describes acceptable **response** encodings. It leaves
already encoded and `no-transform` requests alone. Upstream capability discovery
and removing/regenerating authentication signatures remain proxy responsibilities.
For request compression, `maxDecodedBytes` limits the plain input and
`maxEncodedBytes` limits the compressed output.
Response decompression is also explicit; it does not rewrite the application's
outgoing `Accept-Encoding` policy or synthesize conditional/range requests.

Decoded request bodies and decoded upstream response bodies are buffered **within
limits** before application execution or response headers are committed. This
allows malformed/truncated bodies to fail safely rather than delivering a valid
prefix to an application that might already have performed side effects. It is
not intended for unlimited compressed uploads or indefinite upstream streams.
Incoming bodies without an actual content coding are passed through unchanged;
apply a separate body-limit middleware if identity uploads also need limits.

`encodeBody` and `decodeBody` are convenience helpers. `encodeBody` intentionally
collects its output without a size cap; `encodeBodyBounded` limits output as chunks
are produced. `decodeBody` applies validation, limits, and a deadline.
`decodeBodyWithSize` is the advanced per-layer helper: it takes the *original
wire size* for expansion checks but does not install a deadline itself. Wrap a
multi-layer operation in one deadline, as the middleware does.

## Safety and HTTP behavior

Chunked HTTP/1.1 uploads are supported for every registered codec, including
stacked content codings. WAI/Warp handles transfer framing; this middleware
reads body chunks until EOF and does not require a `Content-Length`. Encoded
size, decoded size, expansion, layer and deadline limits still apply. Requests
are deliberately buffered within those limits and fully validated before the
application is called. The application sees a new `KnownLength`, a correct
`Content-Length`, and no stale `Content-Encoding` or `Transfer-Encoding`.
Thus chunked support does **not** imply unbuffered, incremental delivery of
decoded bytes to the application. Streaming response encoders do emit chunks
incrementally and preserve explicit flushes.

Default request/upstream decoding limits:

| Setting | Default |
| --- | --- |
| Encoded body | 10 MiB |
| Decoded body, including each intermediate layer | 32 MiB |
| Expansion ratio | 100 times original encoded size |
| Expansion slack | 1 MiB |
| Content-encoding layers | 3 |
| Combined read/decode deadline | 5 seconds |

The expansion limit is `originalWireBytes * ratio + slack`, applied independently
to **every layer**, never multiplied per layer. All arithmetic for these limits
uses `Integer`. Input is rechunked into 32 KiB blocks when buffering so very many
tiny network chunks do not retain an unbounded number of full-message chunk
nodes. Output limits are checked before storing each decoder output chunk.

Deadlines use `System.Timeout`, preserve asynchronous cancellation, and are
best-effort: they cannot forcibly preempt an arbitrary non-interruptible FFI call.
Configure Warp/socket timeouts, process/container memory limits, concurrency
limits, and request limits at the deployment boundary as well.

Other behavior:

- `Accept-Encoding` supports case-insensitive tokens, multiple field lines,
  `q` values, wildcard, identity exclusions, and explicit `q=0`. Higher client
  quality wins; the selected server policy breaks ties. Identity defaults to quality 1.
  With no header, the conservative policy is identity (HTTP permits other
  encodings); an empty header also selects identity.
- Malformed weights/parameters fail closed for that coding. Conflicting duplicate
  entries use the minimum quality so exclusions cannot be overridden. Negotiation
  and content-coding headers are capped at 8 KiB.
- No acceptable representation returns `406`; oversized negotiation returns
  `400`. Encoded/raw/bodyless responses are passed through without negotiation.
- `Vary: Accept-Encoding` is merged with existing `Vary` tokens on negotiated
  responses, including identity and bodyless responses, without duplicates; `Vary: *` stays
  unchanged.
- The default 1 KiB threshold uses a valid known length first, otherwise a
  builder's bounded body prefix. Unknown-length streams are compressed when
  eligible. Builder prefix inspection does not force the whole body; it is
  separate from the encoding policy, which never examines body chunks.
  Identity negotiation bypasses both prefix inspection and the encoding policy.
  Compression may
  increase the size of incompressible bodies; there is no whole-message trial
  compression on streaming responses.
- Already encoded responses, raw upgrades, `HEAD`, 1xx, 204, 205, 304, range
  requests, partial responses, and `no-transform` representations are not
  compressed. File responses are opt-in and file slices are never transformed.
  Files are read normally when opted in, so zero-copy serving is lost. There is
  no on-disk precompressed-file cache.
- Default MIME policy covers text, JSON, XML, JavaScript, and SVG; excludes SSE.
  MIME policy is only a performance hint, not a security classification.
- Authorization, cookies, `Set-Cookie`, `private`, and `no-store` exclude response
  compression unless `responseAllowSensitive` is explicitly enabled. **These
  checks do not prove a body contains no secrets.** Application policy remains
  necessary to avoid compression side channels such as BREACH.
- Transformed bodies lose stale `Content-Length`, `Transfer-Encoding`, strong/weak ETags, digest
  fields, `Content-MD5`, and `Accept-Ranges`; buffered bodies get a new correct
  `Content-Length`. Request body length is updated, and transfer framing is
  removed from the normalized request seen by the application.
- Request errors: malformed/truncated coding `400`, unsupported coding `415`
  with supported codecs in `Accept-Encoding`, body/layer/expansion limits `413`,
  deadline `408`. A compressed `no-transform` upload is rejected with `415`.
  Invalid compressed upstream responses return `502` before upstream bytes leak.
- Gzip concatenated members and Zstandard concatenated frames are supported.
  Truncation, trailing junk, invalid headers, and codec failures are rejected.
  Zstandard skippable frames retain the upstream format's semantics.
- Zstandard decoding limits the advertised window to 8 MiB. Explicit high
  compression levels 20 and above also cap their encoder window at 8 MiB;
  compression-level selection does not bypass that interoperability/resource
  limit. Other native allocations still require deployment memory limits.

Put request decoding outside middleware that consumes the request body, and
response compression outside middleware that changes the response. Raw-header
signature verification must happen **before** transformations. Do not use
upstream response decompression around signed, conditional, or range protocols
without designing their representation semantics explicitly.

### Deployment boundaries

The middleware does not install global CPU/concurrency limits or certify an
application's compression side-channel safety. Set service-level capacity,
transport timeouts and native-memory limits independently. Decoded-byte limits
do not bound every native codec allocation, and deadline cancellation cannot
forcibly preempt arbitrary unsafe foreign calls.

Identity uploads need a separate application body-limit policy. Validate actual
responses for user-controlled reflection and secrets; MIME types alone do not
make compression safe. Record chosen codec/profile, transformed byte counts,
duration, quota failures and cancellations through application instrumentation.

The live suite covers local HTTP/1.1 Warp, including chunked uploads. HTTP/2,
HTTP/3 proxying, TLS, slow peers, disconnects, prolonged backpressure and sustained
concurrent load still need deployment-specific validation. The recommended
policy is a replaceable starting point, not a universal production optimum.

## Build and test

```sh
direnv allow
direnv exec . cabal build all --offline -j1
direnv exec . cabal test all --offline -j1 --test-show-details=direct
direnv exec . cabal test all --offline -j1 --ghc-options=-Werror
nix flake check "path:$PWD"
direnv exec . bash tests/check-coverage.sh
direnv exec . bash scripts/check-release.sh --offline
```

Without direnv:

```sh
nix develop "path:$PWD" --command cabal test all --offline -j1 --test-show-details=direct
```

The development flake follows `flake-parts`/`haskell-flake`, provides matching
GHC/HLS tools, discovers packages from `cabal.project`, and exposes the test
package as a Nix check. The path reference also works before creating a Git
repository. On a non-Nix setup, Brotli needs `libbrotlienc` and `libbrotlidec`
visible to `pkg-config`; zlib needs native development files. The `zstd` Haskell
package defaults to building its bundled native implementation.

Tests include RFC negotiation cases, binary QuickCheck roundtrips, interoperability
against each upstream library, byte-at-a-time input, checksums/truncation/trailing
data, concatenated members/frames, quotas, cancellation, deadlines, native window
limits, flushes, file/raw/SSE policy, metadata normalization, proxy transformations,
and real compressed HTTP uploads/responses through Warp.

`tests/check-coverage.sh` builds instrumented libraries and runs the test binary
directly before generating an HPC report. This avoids Cabal's automatic coverage
reporting restriction for a standalone test-only package; tests do not need a
dummy library component.

## Example

```sh
direnv exec . cabal run wai-compression-example
curl -i -H 'Accept-Encoding: br, zstd, gzip' http://127.0.0.1:8080/public
printf 'hello upload' | gzip | curl --data-binary @- -H 'Content-Encoding: gzip' http://127.0.0.1:8080/echo
```

Only `/public` is opted into response compression. `/echo` demonstrates decoded
uploads without compressing reflected user input.

## Author and license

Author: https://github.com/Reykudo.

This project's source code is licensed under MIT; see `LICENSE`.

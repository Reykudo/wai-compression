# Policy benchmark

The runner compares gzip/deflate levels 1, 6, 9; Brotli levels 3, 4, 5, 6, 9;
and Zstandard levels 1, 3, 6, 9. It measures the actual streaming adapters and
bounded decoder, not unrelated command-line encoders.

Supply four UTF-8 corpus files, each at least 64 KiB: `data.json`, `page.html`,
`text-en.txt`, and `text-ru.txt`. The provided downloader fetches public examples
without copying them into this package:

```sh
python3 benchmarks/fetch-corpus.py /tmp/wai-compression-corpus
direnv exec . cabal build exe:wai-compression-benchmarks --offline -j1 --enable-optimization=2
binary=$(direnv exec . cabal list-bin exe:wai-compression-benchmarks --offline)
"$binary" /tmp/wai-compression-corpus > /tmp/wai-compression-results.csv
```

It benchmarks prefixes of 512 B, 1 KiB, 4 KiB, 16 KiB, 64 KiB, 256 KiB and
1 MiB, capped at the available corpus size. JSON/HTML prefixes need not be
complete documents; compression operates on bytes. No body is expanded by
repeating a small seed. Corpus loading, correctness checks and warmup are
outside the timed batches. Each reported encoding/decoding time is the median
of five batches, each calibrated to approximately 10 ms (at most 256 iterations).
GC runs before, not inside, each batch. The CSV also records min/max encoding
times and compressed byte counts. These are wall-clock measurements including
Haskell adapter/buffer overhead, not a throughput or latency SLA.

Run on an otherwise idle host, retain corpus SHA256 hashes, compiler/native
library versions and machine details, and repeat with representative application
payloads. Small CPU-time differences and a single corpus per MIME family are
not evidence of a universal winner. Brotli quality 11 is intentionally omitted
from the on-the-fly response candidates; offline precompression is a different
workload. Streaming flush frequency, concurrent load, native memory usage and
TTFB require separate load tests.

## Initial measurements

Measured on 2026-09-30 on x86_64 Linux, Intel Core i9-13900HX, GHC 9.10.3.
Adapters used `-O1`; the runner used `-O2`. Native libraries were zlib 1.3.2,
Brotli 1.2.0 and Zstandard 1.5.7. The four corpora were the nativejson Twitter
JSON, RFC 9110 HTML and English text, and Russian paragraphs from Tolstoy's
*The Forged Coupon*. All 405 dataset/size/codec/level combinations were
roundtrip-validated before timing.

Representative 64 KiB results show compressed size as a percentage of the input
and median encoding time in microseconds, including adapter/buffer overhead:

| Corpus | gzip 6: size / us | Brotli 5: size / us | zstd 3: size / us |
| --- | --- | --- | --- |
| JSON | 14.78% / 562 | 12.95% / 556 | 16.91% / 99 |
| HTML | 20.12% / 948 | 18.35% / 754 | 21.22% / 135 |
| English text | 32.10% / 2143 | 30.86% / 1182 | 33.68% / 232 |
| Russian text | 28.96% / 3366 | 28.72% / 1149 | 30.83% / 211 |

The recommendation prefers zstd for JSON/general text to reduce serving CPU,
and Brotli for known-size markup of at least 16 KiB as a bandwidth bias.
CSS, JavaScript, XML and SVG are extrapolations from HTML, not directly measured.
The 16 KiB boundary is pragmatic rather than a statistically fitted crossover.
Unknown or smaller bodies use Fast without traversing them to count bytes.
Brotli level 3 took approximately half the encoding time of level 4 on the
measured 4 KiB prefixes, motivating its Fast mapping.

There is only one corpus per family. Prefixes may represent metadata or front
matter rather than typical application content. These are not p95/p99,
concurrency, RSS or network measurements. Repeat with representative application
payloads before changing defaults. Generated CSV files, manifests and downloaded
corpora are not part of the release; keep them with your own benchmark results.
The downloader records transformed UTF-8 hashes and never executes downloaded
data. Corpus files retain their own licenses.

Author: https://github.com/Reykudo. License: MIT; see `LICENSE`.

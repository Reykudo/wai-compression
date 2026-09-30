# wai-compression example

A Warp server with an explicitly compressible public response and a bounded
compressed-upload echo endpoint. Run from the repository root:

```sh
direnv exec . cabal run wai-compression-example
curl -i -H 'Accept-Encoding: br, zstd, gzip' http://127.0.0.1:8080/public
printf 'hello upload' | gzip | curl --data-binary @- \
  -H 'Content-Encoding: gzip' http://127.0.0.1:8080/echo
```

Only `/public` opts into response compression. `/echo` demonstrates request
decoding without enabling compression of reflected user-controlled input.
The example registers every adapter; production applications can use fewer.

Author: https://github.com/Reykudo. License: MIT; see `LICENSE`.

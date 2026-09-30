# wai-compression tests

Behavioral, property, reference-codec and real HTTP/1.1 Warp tests for the core
and all optional adapters. Run from the repository root:

```sh
direnv exec . cabal test all --offline -j1 --ghc-options=-Werror \
  --test-show-details=direct --test-options='--quickcheck-tests=1000 +RTS -N2 -RTS'
direnv exec . bash tests/check-coverage.sh --quickcheck-tests=1000 +RTS -N2 -RTS
```

Tests cover negotiation, response safety, malformed input, resource limits,
profiles, concurrent codec reuse, flushing, cancellation and fragmented chunked
uploads. Native memory, deployment-specific side channels and prolonged load
need application-level validation beyond this suite.

Author: https://github.com/Reykudo. License: MIT; see `LICENSE`.

{
  description = "Modular, bounded WAI body compression";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/1bc55b9def8165e82073919945c3239903fe4dc2";
    flake-parts.url = "github:hercules-ci/flake-parts/31729ca8cbdb4fa927b34e5f4353e6a83f39e993";
    haskell-flake.url = "github:srid/haskell-flake/ec69967e95d7e93b58d9f7cdd384432cb42d37ee";
    brotli = {
      url = "https://hackage-content.haskell.org/package/brotli-0.0.0.3/brotli-0.0.0.3.tar.gz";
      flake = false;
    };
    zstd = {
      url = "https://hackage-content.haskell.org/package/zstd-0.1.4.0/zstd-0.1.4.0.tar.gz";
      flake = false;
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { pkgs, config, ... }: {
        haskellProjects.default = {
          autoWire = [ "packages" "checks" ];
          packages.brotli.source = inputs.brotli;
          packages.zstd.source = inputs.zstd;
          settings.zstd.check = false;
          settings.wai-compression-tests = {
            check = true;
            custom = drv: pkgs.haskell.lib.overrideCabal drv (old: {
              postInstall = (old.postInstall or "") + ''
                mkdir -p "$out"
                cp -r dist/test "$out/test-results"
              '';
            });
          };
          settings.brotli = {
            check = false;
            custom = drv: drv.override { brotli = pkgs.brotli; };
          };
          devShell.tools = hp: {
            inherit (pkgs.haskellPackages) cabal-install haskell-language-server fourmolu;
          };
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [ config.haskellProjects.default.outputs.devShell ];
          packages = [ pkgs.bashInteractive pkgs.pkg-config pkgs.curl pkgs.python3 ];
        };
        checks.wai-compression = config.haskellProjects.default.outputs.finalPackages.wai-compression-tests;
      };
    };
}

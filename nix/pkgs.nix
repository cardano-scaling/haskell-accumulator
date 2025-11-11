{ inputs, self, ... }: {
  perSystem = { config, system, compiler, ... }:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.haskell-nix.overlay
          (final: prev: {
            librust_accumulator = inputs.rust-accumulator.defaultPackage.${final.system};
            haskell-nix = prev.haskell-nix // {
              extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings or { } // {
                "librust_accumulator" = [ "librust_accumulator" ];
                "libblst" = [ "blst" ];
              };
            };
          })
          (final: prev: {
            cabal-install = prev.haskell-nix.tool compiler "cabal-install" "3.10.3.0";
            haskell-language-server = prev.haskell-nix.tool compiler "haskell-language-server" "2.11.0.0";
          })
        ];
        inherit (inputs.haskell-nix) config;
      };
    in
    {
      _module.args = { inherit pkgs; };
    };
}


{
  description = "Haskell bindings for the rust accumulator lib via C";

  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    rust-accumulator.url = "github:cardano-scaling/rust-accumulator";
  };

  outputs = { self, nixpkgs, haskell-nix, CHaP, hackage, rust-accumulator }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" "aarch64-linux" ];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [
            haskell-nix.overlay
            (final: prev: {
              librust_accumulator = rust-accumulator.defaultPackage.${final.system};
              haskell-nix = prev.haskell-nix // {
                extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings or { } // {
                  "librust_accumulator" = [ "librust_accumulator" ];
                  "libblst" = [ "blst" ];
                };
              };
            })
          ];
          inherit (haskell-nix) config;
        }
      );

    in
    {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};

          project = pkgs.haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = "ghc96";
            shell.withHoogle = false;

            inputMap = {
              "https://chap.intersectmbo.org/" = CHaP;
            };
          };

        in
        {
          default = project.hsPkgs.haskell-accumulator.components.library;
          haskell-accumulator = project.hsPkgs.haskell-accumulator.components.library;
        }
      );

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};

          project = pkgs.haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = "ghc96";
            shell.withHoogle = false;

            inputMap = {
              "https://chap.intersectmbo.org/" = CHaP;
            };
          };

        in
        {
          default = project.shellFor {
            tools = {
              cabal = { };
              haskell-language-server = { };
            };

            buildInputs = [
              pkgs.ghcid
            ];
          };
        }
      );
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}

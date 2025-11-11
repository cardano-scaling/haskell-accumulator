{ self, ... }: {
  perSystem = { compiler, inputMap, pkgs, ... }:
    let
      hsPkgs = pkgs.haskell-nix.project {
        src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
          name = "haskell-accumulator";
          src = self;
          filter = path: _type:
            builtins.all (x: baseNameOf path != x) [
              "flake.nix"
              "flake.lock"
              "nix"
              ".github"
            ];
        };
        projectFileName = "cabal.project";
        inherit inputMap;
        compiler-nix-name = compiler;
      };
    in
    {
      _module.args = { inherit hsPkgs; };
    };
}


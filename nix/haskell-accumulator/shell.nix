{ self, ... }: {
  perSystem = { pkgs, hsPkgs, ... }:
    let
      buildInputs = [
        pkgs.cabal-install
        pkgs.haskell-language-server
        pkgs.ghcid
      ];
    in
    {
      devShells = {
        default = hsPkgs.shellFor {
          buildInputs = buildInputs;
        };
      };
    };
}


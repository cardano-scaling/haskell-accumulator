{ self, ... }: {
  perSystem = { pkgs, hsPkgs, ... }: {
    packages = {
      default = hsPkgs.haskell-accumulator.components.library;
      haskell-accumulator = hsPkgs.haskell-accumulator.components.library;
    };
  };
}


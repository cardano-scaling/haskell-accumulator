{ inputs, ... }: {
  perSystem = { pkgs, system, ... }:
    let
      compiler = "ghc96";
      inputMap = { "https://chap.intersectmbo.org/" = inputs.CHaP; };
    in
    {
      _module.args = { inherit compiler inputMap; };
    };
}


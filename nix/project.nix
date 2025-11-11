{ repoRoot, inputs, pkgs, lib, system }:

let

  cabalProject = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }: {
    name = "haskell-accumulator";
    src = ../.;
    compiler-nix-name = "ghc96";
    shell.withHoogle = false;
    inputMap = {
      "https://chap.intersectmbo.org/" = inputs.CHaP;
    };
  });

  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    inherit system;
    shellArgs = repoRoot.nix.shell;
  };

in

project

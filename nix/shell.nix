{ repoRoot, inputs, pkgs, system, lib }:

cabalProject:

{
  name = "haskell-acummulator";

  packages = lib.traceSeq inputs.CHaP [
  ];

  preCommit = {
    cabal-fmt.enable = true;
    cabal-fmt.extraOptions = "--no-tabular";
    nixpkgs-fmt.enable = true;
    shellcheck.enable = true;
    fourmolu.enable = true;
    fourmolu.extraOptions = "-o -XCPP";
    hlint.enable = true;
  };

  tools = { };

  scripts = { };

  shellHook = ''
  '';
}

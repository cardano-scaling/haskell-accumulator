{ repoRoot, inputs, pkgs, system, lib }:
let
  project = repoRoot.nix.project;
in
[
  (project.flake)
  {
    packages.default = project.cabalProject.hsPkgs.haskell-accumulator.components.library;
    devShells.default = project.devShell;
  }
]

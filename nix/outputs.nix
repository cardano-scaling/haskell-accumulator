{ repoRoot, inputs, pkgs, system, lib }:
let
  project = repoRoot.nix.project;
in
{
  packages = {
    default = project.cabalProject.hsPkgs.haskell-accumulator.components.library;
    haskell-accumulator = project.cabalProject.hsPkgs.haskell-accumulator.components.library;
  };

  devShells = {
    default = project.devShell;
  };
}

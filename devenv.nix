{pkgs, ...}: {
  # https://devenv.sh/basics/
  packages = [
    pkgs.git
    pkgs.graphviz
  ];

  languages.nix.enable = true;
  languages.haskell.enable = true;

  pre-commit.hooks.deadnix.enable = true;
  pre-commit.hooks.ormolu.enable = true;
}

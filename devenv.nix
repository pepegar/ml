{pkgs, ...}: {
  packages = [
    pkgs.git
  ];

  languages.nix.enable = true;
  languages.haskell.enable = true;
  languages.python.enable = true;
  languages.python.poetry.enable = true;
  languages.python.poetry.activate.enable = true;

  pre-commit.hooks.deadnix.enable = true;
  pre-commit.hooks.ormolu.enable = true;
}

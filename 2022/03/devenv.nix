{ pkgs, ... }:

{
  env.GREET = "aoc";

  packages = [ pkgs.ghc ];

  languages.haskell.enable = true;
}

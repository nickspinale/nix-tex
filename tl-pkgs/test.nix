with (import <nixpkgs> {});
let
  pkgs = import ./pkgs.nix {};
  tl-pkgs = callPackage (import ./default.nix) {};
in
  callPackage (tl-pkgs.tlify "kpathsea.x86_64-linux" pkgs."kpathsea.x86_64-linux") {}

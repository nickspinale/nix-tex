with (import <nixpkgs> {});
let
  pkgs = import ./pkgs.nix {};
  tl-pkgs = callPackage (import ./default.nix) {};
in
  tl-pkgs.tlify "microtype" pkgs.microtype

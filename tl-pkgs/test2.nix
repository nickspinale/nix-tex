with (import <nixpkgs> {});
let
  def = callPackage (import ./default.nix) {};
in
  def."pdftex.x86_64-linux"

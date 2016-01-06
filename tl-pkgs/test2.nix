with (import <nixpkgs> {});
let
  def = callPackage (import ./default.nix) {};
in
  def.microtype

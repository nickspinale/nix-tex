{ nixpkgs ? import <nixpkgs> {} }:

{
  tl-progs = nixpkgs.pkgs.callPackage (import ./tl-progs) {};
}

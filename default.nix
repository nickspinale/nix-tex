{ pkgs ? import <nixpkgs> {} }:

with pkgs;

{ tl-progs = callPackage (import ./tl-progs) {};
  tl-pkgs =  callPackage (import ./tl-pkgs) {};
}

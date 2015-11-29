{ nixpkgs ? import <nixpkgs> {} }:

{
  tl-progs = import ./tl-progs {
    stdenv = nixpkgs.pkgs.stdenv;
    fetchurl = nixpkgs.pkgs.fetchurl;
  };
}

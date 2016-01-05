{ nixpkgs ? (import <nixpkgs> {}) }:

rec {

  tl-source-tarball = nixpkgs.pkgs.fetchurl {
    url = ftp://tug.org/texlive/historic/2015/texlive-20150521-source.tar.xz;
    sha256 = "ed9bcd7bdce899c3c27c16a8c5c3017c4f09e1d7fd097038351b72497e9d4669";
  };

  tl-source = derivation {
    system = builtins.currentSystem;
    name = "tl-source";
    src = tl-source-tarball;
    builder = ./tl-source-builder.sh;
    gnutar = nixpkgs.pkgs.gnutar;
  };

}

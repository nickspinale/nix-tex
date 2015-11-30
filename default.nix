{ nixpkgs ? import <nixpkgs> {} }:

{
  tl-progs = nixpkgs.pkgs.callPackage (import ./tl-progs) {};
  /* src = fetchurl { */
  /*   url = http://tug.org/svn/texlive/trunk/Master/source/ */
  /*   sha256 = "ed9bcd7bdce899c3c27c16a8c5c3017c4f09e1d7fd097038351b72497e9d4669"; */
  /*   md5 = "0sa6kmz4jwhv6lw702gxszhhjkvw071wba0ngk1c76g8vixwv6zd"; */
  /* }; */
}

{ stdenv
, fetchurl
, perl
, fontconfig
, xlibs
}:

stdenv.mkDerivation {
  name = "tl-progs-0.1.0.0";
  builder = ./builder.sh;
  buildInputs = [ perl fontconfig xlibs.libX11 xlibs.libXmu xlibs.libXaw ];
  src = fetchurl {
    url = ftp://tug.org/texlive/historic/2015/texlive-20150521-source.tar.xz;
    sha256 = "ed9bcd7bdce899c3c27c16a8c5c3017c4f09e1d7fd097038351b72497e9d4669";
    /* md5 = "0sa6kmz4jwhv6lw702gxszhhjkvw071wba0ngk1c76g8vixwv6zd"; */
  };
}

source $stdenv/setup

tar xf $src

mkdir tmp
cd tmp

args="--disable-native-texlive-build"
TL_INSTALL_DEST=$out TL_CONFIGURE_ARGS=$args ../texlive-20150521-source/Build

rm -r $out/share/texmf-dist

source $stdenv/setup

tar xf $src

mkdir wat
cd wat

args="--disable-native-texlive-build"
# args="--without-x --disable-native-texlive-build"
# args="--disable-all-pkgs --enable-tex --enable-mf --without-mf-x-toolkit --without-x"

TL_INSTALL_DEST=$out TL_CONFIGURE_ARGS=$args ../texlive-20150521-source/Build

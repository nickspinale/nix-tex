source $stdenv/setup

tar xf $src

mkdir wat
cd wat

args="--disable-native-texlive-build"
# args="--without-x --disable-native-texlive-build"
# args="--disable-all-pkgs --enable-tex --enable-mf --without-mf-x-toolkit --without-x"

TL_INSTALL_DEST=$out TL_CONFIGURE_ARGS=$args ../texlive-20150521-source/Build

mv $out/share/texmf-dist/web2c/texmf.cnf .
rm -r $out/share/texmf-dist
mkdir -p $out/share/texmf-dist/web2c
mv texmf.cnf $out/share/texmf-dist/web2c

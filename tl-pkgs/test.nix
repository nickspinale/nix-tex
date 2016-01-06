with (import <nixpkgs> {});
let
  urlPrefix = "https://www.ctan.org/tex-archive/systems/texlive/tlnet/archive/";
  urlSuffix = ".tar.xz";
  name = "microtype";
  md5 = { default = "aae3ee3d275745e494572d428bc96890";
          src = "500f157d8ef3b25e3d4f741d3689dc13";
          doc = "077937761c10032146e6ed8de7bd9868";
        };
in
  stdenv.mkDerivation {
    inherit name;
    builder = ./tl-pkgs-builder.sh;
    default = fetchurl { url = urlPrefix + name + urlSuffix; md5 = md5.default; };
    src = fetchurl { url = urlPrefix + name + ".source" + urlSuffix; md5 = md5.src; };
    doc = fetchurl { url = urlPrefix + name + ".doc" + urlSuffix; md5 = md5.doc; };
  }

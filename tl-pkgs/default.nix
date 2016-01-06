{ stdenv, lib, fetchurl, callPackage }:
{
  tlify = name: { relocated, deps, md5 ? {} } :
    let
      urlPrefix = "https://www.ctan.org/tex-archive/systems/texlive/tlnet/archive";
      urlSuffix = ".tar.xz";
    in
      stdenv.mkDerivation ({
         inherit name;
         builder = ./tl-pkgs-builder.sh;
      } // (if builtins.hasAttr "default" md5
            then { default = fetchurl { url = urlPrefix + name + urlSuffix; md5 = md5.defualt; }; }
            else {})
        // (if builtins.hasAttr "src" md5
            then { src = fetchurl { url = urlPrefix + name + ".src" + urlSuffix; md5 = md5.src; }; }
            else {})
        // (if builtins.hasAttr "doc" md5
            then { doc = fetchurl { url = urlPrefix + name + ".doc" + urlSuffix; md5 = md5.doc; }; }
            else {})
      );
}

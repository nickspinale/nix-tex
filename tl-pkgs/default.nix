{ stdenv, fetchurl, patchelf, glibc, callPackage }:
let
  fix = f: let x = f x; in x;
  pkgs = import ./pkgs.nix stdenv.system;
  names = builtins.attrNames pkgs;
  tl = self:
    let
      f = name:
        with builtins.getAttr name pkgs;
        let
          urlPrefix = "https://www.ctan.org/tex-archive/systems/texlive/tlnet/archive/" + name;
          urlSuffix = ".tar.xz";
          value = stdenv.mkDerivation ({
              inherit name glibc;
              builder = ./tl-pkgs-builder.sh;
              buildInputs = [ patchelf ];
              transform = if relocated then "--transform=s,^,texmf-dist/," else "";
              tl-deps = map (dep: builtins.getAttr dep self) deps;
            } // (if builtins.hasAttr "default" md5
                  then { default = fetchurl { url = urlPrefix + urlSuffix; md5 = md5.default; }; }
                  else {})
              // (if builtins.hasAttr "src" md5
                  then { src = fetchurl { url = urlPrefix + ".source" + urlSuffix; md5 = md5.src; }; }
                  else {})
              // (if builtins.hasAttr "doc" md5
                  then { doc = fetchurl { url = urlPrefix + ".doc" + urlSuffix; md5 = md5.doc; }; }
                  else {})
            );
        in
          { inherit name value; };
    in
      builtins.listToAttrs (map f names);
in
  fix tl

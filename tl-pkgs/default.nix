{ stdenv, fetchurl, patchelf, utillinux, callPackage }:
let
  fix = f: let x = f x; in x;
  pkgs = import ./pkgs.nix stdenv.system;
  names = builtins.attrNames pkgs;
  tl = self:
    let
      f = name:
        let
          pkg = builtins.getAttr name pkgs;
          urlPrefix = "https://www.ctan.org/tex-archive/systems/texlive/tlnet/archive/" + name;
          urlSuffix = ".tar.xz";
          value = stdenv.mkDerivation ({
              inherit name;
              builder = ./tl-pkgs-builder.sh;
              buildInputs = [ patchelf utillinux ];
              transform = if pkg.relocated then "--transform=s,^,texmf-dist/," else "";
              tl-deps = map (dep: builtins.getAttr dep self) pkg.depend;
            } // (if builtins.hasAttr "containermd5" pkg
                  then { default = fetchurl { url = urlPrefix + urlSuffix; md5 = pkg.containermd5; }; }
                  else {})
              // (if builtins.hasAttr "srccontainermd5" pkg
                  then { src = fetchurl { url = urlPrefix + ".source" + urlSuffix; md5 = pkg.srccontainermd5; }; }
                  else {})
              // (if builtins.hasAttr "doccontainermd5" pkg
                  then { doc = fetchurl { url = urlPrefix + ".doc" + urlSuffix; md5 = pkg.doccontainermd5; }; }
                  else {})
            );
        in
          { inherit name value; };
    in
      builtins.listToAttrs (map f names);
in
  fix tl

nix-build tl2nix > /dev/null
(curl tug.ctan.org/systems/texlive/tlnet/tlpkg/texlive.tlpdb | ./result/bin/tl2nix) > pkgs.nix

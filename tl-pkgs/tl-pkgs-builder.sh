source $stdenv/setup

mkdir $out

for kind in $default $src $doc; do
    if test -f $kind; then
        tar $transform -C $out --keep-old-files --exclude='tlpkg/*' -xf $kind 
    fi
done

for file in $(find $out -type f); do
    if test $(hexdump -n 4 -e '"%x"' $file) = 464c457f; then
        patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" $file
    fi
done


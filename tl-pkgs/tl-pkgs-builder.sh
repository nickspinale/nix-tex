source $stdenv/setup

mkdir $out

for kind in $default $src $doc; do
    if test -f $kind; then
        tar $transform -C $out --keep-old-files --exclude='tlpkg/*' -xf $kind 
    fi
done

for file in $(find $out * \( -type f -a -executable \)); do
    patchelf --set-interpreter $glibc$(patchelf --print-interpreter $file) $file
done


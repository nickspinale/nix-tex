source $stdenv/setup

mkdir $out

for kind in $default $src $doc; do
    if test -f $kind; then
        tar $transform -C $out --keep-old-files --exclude='tlpkg/*' -xf $kind 
    fi
done

# find "$out" \( \
#     \( -type f -a -name "*.so*" \) -o \
#     \( -type f -a -perm -0100 \) \
#     \) -print -exec patchelf --shrink-rpath '{}' \;

for file in $(find $out * \( -type f -a -executable \)); do
    current=$(patchelf --print-interpreter $file)
    # if test $? -eq 0; then
    patchelf --set-interpreter $glibc$current $file
    # fi
done

# for file in $(find $out * -type f); do
#     current=$(patchelf --print-interpreter $file)
#     if test $? -eq 0; then
#         patchelf --set-interpreter $glibc$current $file
#     fi
# done


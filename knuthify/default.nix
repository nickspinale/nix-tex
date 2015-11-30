{ name }:

derivation {
  inherit name;
  builder = ./builder.sh;
}

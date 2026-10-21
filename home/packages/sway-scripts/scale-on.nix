{
  writeShellScript,
  sway,
  coreutils,
}:

let
  swaymsg = "${sway}/bin/swaymsg";
  cat = "${coreutils}/bin/cat";
in
writeShellScript "scale-on" ''
  for scale_file in /tmp/scale-on/*; do
    ${swaymsg} "output \"$(basename "$scale_file")\" scale $(${cat} "$scale_file")"
  done
''

{
  lib,
  writeShellScript,
  sway,
  jq,
}:

let
  swaymsg = "${sway}/bin/swaymsg";
in
writeShellScript "scale-off" ''
  rm -rf /tmp/scale-on
  mkdir /tmp/scale-on

  while read -r scale name; do
    echo "$scale" > "/tmp/scale-on/$name";
  done < <(${swaymsg} -r -t get_outputs | ${lib.getExe jq} -r '.[] | "\(.scale) \(.make) \(.model) \(.serial)"')

  ${swaymsg} 'output * scale 1'
''

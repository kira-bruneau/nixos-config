{
  lib,
  writeShellScriptBin,
  waybar,
}:

writeShellScriptBin "waybar-dynamic-css" ''
  echo "" > ~/.config/waybar/dynamic.css
  exec ${lib.getExe waybar}
''

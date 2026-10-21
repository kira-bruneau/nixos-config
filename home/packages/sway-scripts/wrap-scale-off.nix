{
  writeShellScriptBin,
  scale-off,
  scale-on,
}:

writeShellScriptBin "wrap-scale-off" ''
  ${scale-off}
  export MANGOHUD_CONFIGFILE=$HOME/.config/MangoHud/MangoHud-HiDPI.conf
  "$1" "''${@:2}"
  ${scale-on}
''

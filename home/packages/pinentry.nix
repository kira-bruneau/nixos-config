{
  lib,
  writeShellScriptBin,
  pinentry-gnome3,
  pinentry-tty,
}:

writeShellScriptBin "pinentry" ''
  if [ -n "$DISPLAY" ]; then
    exec ${lib.getExe pinentry-gnome3} "$@"
  fi

  exec ${lib.getExe pinentry-tty} "$@"
''

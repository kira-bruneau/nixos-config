{
  writeShellScript,
  vorbis-tools,
  sound-theme-freedesktop,
}:

writeShellScript "sound" ''
  exec ${vorbis-tools}/bin/ogg123 "${sound-theme-freedesktop}/share/sounds/freedesktop/stereo/$1.oga"
''

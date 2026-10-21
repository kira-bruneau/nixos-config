{
  lib,
  writeShellScript,
  random-wallpaper,
  systemd,
  swaylock,
}:

let
  systemctl = "${systemd}/bin/systemctl";
in
writeShellScript "lock" ''
  args=(--daemonize --image `${random-wallpaper}`)

  ${systemctl} is-enabled fprintd.service
  if [ $? -le 1 ]; then
    args+=(--fingerprint)
  fi

  exec ${lib.getExe swaylock} "''${args[@]}"
''

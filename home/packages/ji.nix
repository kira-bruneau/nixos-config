{ writeShellApplication, systemd }:

writeShellApplication {
  name = "ji";
  runtimeInputs = [ systemd ];
  text = ''
    journalctl -I -u "$@"
  '';
}

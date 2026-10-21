{ writeShellApplication, systemd }:

writeShellApplication {
  name = "jb";
  runtimeInputs = [ systemd ];
  text = ''
    if [ $# -gt 0 ]; then
      journalctl -b -u "$@"
    else
      journalctl -b
    fi
  '';
}

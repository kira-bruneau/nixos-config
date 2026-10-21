{ writeShellApplication, coreutils }:

writeShellApplication {
  name = "sudir";
  runtimeInputs = [ coreutils ];
  text = ''
    exec sudo -u "$(stat -c "%U" "$1" 2>/dev/null || echo 'root')" sh -c \
      'cd $1 && exec env HOME=/var/empty $2' sudir "$1" "$SHELL"
  '';
}

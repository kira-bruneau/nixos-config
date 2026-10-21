{
  writeShellApplication,
  coreutils,
  which,
}:

writeShellApplication {
  name = "wf";

  runtimeInputs = [
    coreutils
    which
  ];

  text = ''
    readlink -f "$(which "$@")"
  '';
}

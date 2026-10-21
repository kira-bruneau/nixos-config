{
  writeShellApplication,
  nix,
  coreutils,
  git,
  jq,
}:

writeShellApplication {
  name = "nid";

  bashOptions = [ ];

  runtimeInputs = [
    nix
    coreutils
    git
    jq
  ];

  text = builtins.replaceStrings [ "nid.cmake" ] [ "${./nid.cmake}" ] (builtins.readFile ./nid.sh);
}

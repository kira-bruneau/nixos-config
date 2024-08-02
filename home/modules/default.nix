{
  imports = builtins.concatMap (module: if module != "default.nix" then [ ./${module} ] else [ ]) (
    builtins.attrNames (builtins.readDir ./.)
  );
}

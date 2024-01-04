{ inputs, lib, config, pkgs, ... }:

with lib;

{
  options.home.configDirectory = mkOption {
    type = types.path;
    default = "${config.home.homeDirectory}/Dev/public/nixos-config/home";
  };

  config.home.activation.seedHomeConfig =
    let
      configDirectory = escapeShellArg config.home.configDirectory;
    in
    hm.dag.entryAfter [ "writeBoundary" ] ''
      if [ ! -e ${configDirectory} ]; then
        $DRY_RUN_CMD mkdir -p $VERBOSE_ARG $(dirname ${configDirectory})
        $DRY_RUN_CMD cp -R --no-preserve=ownership $VERBOSE_ARG ${inputs.self} ${configDirectory}
        $DRY_RUN_CMD chmod -R +w $VERBOSE_ARG ${configDirectory}
        $DRY_RUN_CMD cd ${configDirectory}
        $DRY_RUN_CMD ${pkgs.git}/bin/git init -b main
        $DRY_RUN_CMD ${pkgs.git}/bin/git config include.path ../.gitconfig
      fi
    '';
}

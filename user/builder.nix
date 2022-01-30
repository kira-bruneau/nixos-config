{ config, pkgs, ... }:

{
  users = {
    users.builder = {
      isSystemUser = true;
      useDefaultShell = true;
      createHome = true;
      home = "/home/builder";
      group = "builder";
    };

    groups.builder = {};
  };

  nix.settings.trusted-users = [ "builder" ];
  services.xserver.displayManager.hiddenUsers = [ "builder" ];
}

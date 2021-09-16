{ config, pkgs, ... }:

{
  users.users.builder = {
    isSystemUser = true;
    useDefaultShell = true;
    createHome = true;
    home = "/home/builder";
    group = "builder";
  };

  nix.trustedUsers = [ "builder" ];
  services.xserver.displayManager.hiddenUsers = [ "builder" ];
}

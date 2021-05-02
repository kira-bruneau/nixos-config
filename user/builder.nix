{ config, pkgs, ... }:

{
  users.users.builder = {
    isSystemUser = true;
    useDefaultShell = true;
    createHome = true;
    home = "/home/builder";
  };

  nix.trustedUsers = [ "builder" ];
  services.xserver.displayManager.hiddenUsers = [ "builder" ];
}

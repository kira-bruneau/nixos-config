{ config, pkgs, ... }:

{
  users.users.builder = {
    uid = 1001;
    shell = pkgs.bashInteractive;
    createHome = true;
    home = "/home/builder";
  };

  nix.trustedUsers = [ "builder" ];
}

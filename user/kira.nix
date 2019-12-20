{ config, pkgs, ... }:

{
  users.users.kira = {
    uid = 1000;
    description = "Kira Bruneau";
    extraGroups = [ "wheel" "docker" "adbusers" ];
    isNormalUser = true;
  };

  # TODO: Automatically clone Home Manager configuration
}

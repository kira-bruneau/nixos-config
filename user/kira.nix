{ config, pkgs, ... }:

{
  users.users.kira = {
    isNormalUser = true;
    description = "Kira Bruneau";
    extraGroups = [ "wheel" "docker" "adbusers" ];
  };

  # TODO: Automatically clone Home Manager configuration
}

{ config, pkgs, ... }:

{
  users.users.kira = {
    isNormalUser = true;
    description = "Kira Bruneau";
    extraGroups = [ "wheel" "adbusers" ];
  };

  # TODO: Automatically clone Home Manager configuration
}

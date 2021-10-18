{ config, pkgs, ... }:

{
  imports = [
    ../environment/colemak.nix
  ];

  users.users.kira = {
    isNormalUser = true;
    description = "Kira Bruneau";
    extraGroups = [ "wheel" "adbusers" ];
    initialPassword = "kira";
  };
}

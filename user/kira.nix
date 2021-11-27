{ config, pkgs, ... }:

{
  imports = [
    ../environment/colemak.nix
    ../group/audio.nix
  ];

  users.users.kira = {
    isNormalUser = true;
    description = "Kira Bruneau";
    extraGroups = [ "wheel" "adbusers" "audio" ];
    initialPassword = "kira";
  };
}

{ config, pkgs, ... }:

{
  imports = [
    ../environment/colemak
    ../group/audio.nix
  ];

  users.users.kira = {
    isNormalUser = true;
    description = "Kira Bruneau";
    extraGroups = [ "wheel" "adbusers" "audio" "video" ];
    initialPassword = "kira";
  };
}

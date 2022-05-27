{ lib, pkgs, ... }:

{
  imports = [
    ../environment/cli.nix
    ../environment/gui.nix
  ];

  home.packages = with pkgs; [
    arctype
    vscodium
  ];

  services.syncthing.enable = true;

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "arctype"
    "unrar"
  ];

  programs.firefox.profiles.kira.settings = {
    "media.ffmpeg.vaapi.enabled" = true;
  };

  home.stateVersion = "21.11";
}

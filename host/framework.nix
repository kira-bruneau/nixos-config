{ lib, pkgs, ... }:

{
  imports = [
    ../environment/cli.nix
    ../environment/gui.nix
  ];

  home.packages = with pkgs; [
    arctype
  ];

  services.syncthing.enable = true;

  # Required by arctype to manage passwords
  services.gnome-keyring.enable = true;

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "arctype"
    "slack"
    "unrar"
  ];

  programs.firefox.profiles.kira.settings = {
    "media.ffmpeg.vaapi.enabled" = true;
  };

  home.stateVersion = "21.11";
}

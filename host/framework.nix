{ lib, ... }:

{
  imports = [
    ../environment/cli.nix
    ../environment/gui.nix
  ];

  services.syncthing.enable = true;

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "slack"
    "unrar"
  ];

  programs.firefox.profiles.kira.settings = {
    "media.ffmpeg.vaapi.enabled" = true;
  };

  home.stateVersion = "21.11";
}

{ lib, pkgs, ... }:

{
  imports = [
    ../environments/dev.nix
    ../environments/gui/sway.nix
    ../environments/laptop.nix
    ../environments/media-production.nix
    ../environments/office.nix
  ];

  home.stateVersion = "21.11";

  programs = {
    mpv.config = {
      # Hardware acceleration
      hwdec = "vaapi";

      # Fix stuttering playing 4k video
      hdr-compute-peak = "no";
    };

    waybar.settings.mainBar.temperature.thermal-zone = 5;
  };
}

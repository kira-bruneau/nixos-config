{ lib, pkgs, ... }:

{
  imports = [
    ../environment/laptop.nix
  ];

  programs = {
    mpv.config = {
      # Hardware acceleration
      hwdec = "vaapi";

      # Fix stuttering playing 4k video
      hdr-compute-peak = "no";
    };

    waybar.settings.mainBar.temperature.thermal-zone = 5;
  };

  nixpkgs.config = {
    allowUnfreePredicate = pkg: builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
      "anytype"
      "anytype-heart"
      "discord"
      "unrar"
    ];

    permittedInsecurePackages = [
      "electron-24.8.6"
    ];
  };

  home.stateVersion = "21.11";
}

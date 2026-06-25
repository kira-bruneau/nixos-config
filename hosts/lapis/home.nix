{ lib, pkgs, ... }:

{
  imports = [
    ../../home/environments/art.nix
    ../../home/environments/dev
    ../../home/environments/gui/sway.nix
    ../../home/environments/laptop.nix
    ../../home/environments/office.nix
    ../../home/programs/prismlauncher
    ../../home/programs/steam
    ../../home/programs/tsukimi
  ];

  home = {
    stateVersion = "24.05";

    sessionVariables = {
      # Hardware acceleration for gstreamer
      GST_PLUGIN_SYSTEM_PATH_1_0 = lib.makeSearchPathOutput "lib" "lib/gstreamer-1.0" (
        with pkgs.gst_all_1; [ gst-vaapi ]
      );
    };
  };

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

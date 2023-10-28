{ lib, pkgs, ... }:

{
  imports = [
    ../environment/cli.nix
    ../environment/gui.nix
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

  services.swayidle = {
    timeouts = [
      {
        timeout = 600;
        command = "/run/current-system/systemd/bin/systemctl suspend-then-hibernate";
      }
    ];
  };

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
    "Anytype"
    "discord"
    "unrar"
  ];

  home.stateVersion = "21.11";
}

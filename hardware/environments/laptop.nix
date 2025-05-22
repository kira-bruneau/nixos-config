{
  lib,
  config,
  pkgs,
  ...
}:

let
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
in
{
  imports = [
    ./bluetooth.nix
    ./portable.nix
    ./wifi.nix
  ];

  # Automatically control frequency of CPU to save power
  services.auto-cpufreq.enable = !config.services.power-profiles-daemon.enable;

  # Disable tlp being enabled from common-pc-laptop in nixos-hardware
  services.tlp.enable = false;

  programs.dconf.profiles = lib.mkIf config.services.xserver.desktopManager.gnome.enable {
    user.databases = [
      {
        settings = {
          "org/gnome/desktop/peripherals/touchpad" = {
            natural-scroll = true;
          };
        };
      }
    ];
  };
}

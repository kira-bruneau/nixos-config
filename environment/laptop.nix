{ config, pkgs, ... }:

{
  imports = [
    ./desktop.nix
    ./wireless.nix
  ];

  # Enable touchpad support
  services.xserver.libinput = {
    enable = true;
    touchpad = {
      accelProfile = "flat";
      naturalScrolling = true;
    };
  };

  # Power management for saving laptop battery
  services.tlp.enable = true;

  # Automatically suspend on low power
  services.upower.enable = true;

  # Enable light for controlling backlight
  programs.light.enable = true;
}

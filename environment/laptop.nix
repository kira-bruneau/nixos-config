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

  # Automatically control frequency of CPU to save power
  powerManagement.cpuFreqGovernor = "powersave";

  # Enable light for controlling backlight
  programs.light.enable = true;
}

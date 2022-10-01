{ config, pkgs, ... }:

{
  # Enable wifi support
  networking.wireless.enable = true;

  # Enable bluetooth support
  hardware.bluetooth.enable = true;
}

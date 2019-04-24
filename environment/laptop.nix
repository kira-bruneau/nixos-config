{ config, pkgs, ... }:

{
  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  # Enable wireless support.
  networking.wireless.enable = true;
}

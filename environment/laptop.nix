{ config, pkgs, ... }:

{
  # Enable touchpad support.
  services.xserver.libinput.enable = true;
}

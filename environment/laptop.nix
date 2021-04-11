{ config, pkgs, ... }:

{
  imports = [
    ./wireless.nix
  ];

  # Enable touchpad support
  services.xserver.libinput.enable = true;
}

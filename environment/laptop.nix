{ config, pkgs, ... }:

{
  imports = [
    ./desktop.nix
    ./wireless.nix
  ];

  # Enable touchpad support
  services.xserver.libinput.enable = true;
}

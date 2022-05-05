{ config, pkgs, ... }:

{
  imports = [
    ./desktop.nix
    ./wireless.nix
  ];

  # Enable touchpad support
  services.xserver.libinput.enable = true;

  # Automatically control frequency of CPU to save power
  powerManagement.cpuFreqGovernor = "powersave";
}

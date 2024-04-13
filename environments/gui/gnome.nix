{ pkgs, ... }:

{
  imports = [
    ./.
  ];

  services.xserver = {
    desktopManager.gnome.enable = true;
    excludePackages = with pkgs; [
      xterm
    ];
  };

  services.gnome.core-utilities.enable = false;
  environment.gnome.excludePackages = with pkgs; [
    gnome-tour
  ];

  networking.networkmanager.enable = true;
}

{
  imports = [
    ./.
  ];

  services.xserver.desktopManager.gnome.enable = true;
  services.gnome.core-utilities.enable = false;
  networking.networkmanager.enable = true;
}

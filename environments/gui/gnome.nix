{
  imports = [
    ./.
  ];

  services.xserver.desktopManager.gnome.enable = true;
  networking.networkmanager.enable = true;
}

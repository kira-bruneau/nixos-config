{ config, lib, pkgs, ... }:

{
  imports = [
    ../environments/gaming.nix
    ../environments/gui/gnome.nix
    ../environments/laptop.nix
  ];

  home.stateVersion = "23.11";

  dconf.settings = {
    "org/gnome/shell" = {
      favorite-apps = [
        "org.gnome.Nautilus.desktop"
        "firefox.desktop"
        "jackfox.desktop"
        "steam.desktop"
        "com.github.iwalton3.jellyfin-media-player.desktop"
        "org.gnome.Console.desktop"
        "dev.vlinkz.NixSoftwareCenter.desktop"
      ];
    };
  };
}

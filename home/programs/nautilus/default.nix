{ lib, pkgs, ... }:

{
  home.packages = with pkgs; [ gnome.nautilus ];

  dconf.settings = {
    "org/gnome/nautilus/icon-view" = {
      default-zoom-level = "small-plus";
    };
  };

  xdg.mimeApps.defaultApplications = lib.mkDefault { "inode/directory" = "nautilus.desktop"; };
}

{ lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    gnome.nautilus
  ];

  xdg.mimeApps.defaultApplications = lib.mkDefault {
    "inode/directory" = "nautilus.desktop";
  };
}

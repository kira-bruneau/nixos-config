{
  lib,
  pkgs,
  pkgsKiraNur,
  ...
}:

{
  home = {
    packages = with pkgs; [
      ffmpegthumbnailer
      nautilus
      pkgsKiraNur.nautilus-taildrop
    ];
  };

  dconf.settings = {
    "org/gnome/nautilus/icon-view" = {
      default-zoom-level = "small-plus";
    };
  };

  xdg.mimeApps.defaultApplications = lib.mkDefault { "inode/directory" = "nautilus.desktop"; };
}

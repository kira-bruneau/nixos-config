{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gnome3.eog
  ];

  xdg.mimeApps.defaultApplications = {
    "image/bmp" = "org.gnome.eog.desktop";
    "image/gif" = "org.gnome.eog.desktop";
    "image/jpeg" = "org.gnome.eog.desktop";
    "image/jpg" = "org.gnome.eog.desktop";
    "image/pjpeg" = "org.gnome.eog.desktop";
    "image/png" = "org.gnome.eog.desktop";
    "image/svg+xml" = "org.gnome.eog.desktop";
    "image/svg+xml-compressed" = "org.gnome.eog.desktop";
    "image/tiff" = "org.gnome.eog.desktop";
    "image/vnd.wap.wbmp" = "org.gnome.eog.desktop";
    "image/x-bmp" = "org.gnome.eog.desktop";
    "image/x-gray" = "org.gnome.eog.desktop";
    "image/x-icb" = "org.gnome.eog.desktop";
    "image/x-icns" = "org.gnome.eog.desktop";
    "image/x-ico" = "org.gnome.eog.desktop";
    "image/x-pcx" = "org.gnome.eog.desktop";
    "image/x-png" = "org.gnome.eog.desktop";
    "image/x-portable-anymap" = "org.gnome.eog.desktop";
    "image/x-portable-bitmap" = "org.gnome.eog.desktop";
    "image/x-portable-graymap" = "org.gnome.eog.desktop";
    "image/x-portable-pixmap" = "org.gnome.eog.desktop";
    "image/x-xbitmap" = "org.gnome.eog.desktop";
    "image/x-xpixmap" = "org.gnome.eog.desktop";
  };
}
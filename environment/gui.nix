{ pkgs, ... }:

{
  imports = [
    ../package/firefox
    ../package/gtk
    ../package/i3
    ../package/keepassxc
    ../package/lmms
    ../package/speedcrunch
    ../package/sway
    ../package/newsflash
  ];

  home.packages = with pkgs; [
    # Administration
    pavucontrol

    # Web
    chromium

    # Media & Documents
    audacity
    blender
    evince
    freetube
    gimp
    gnome3.eog
    gnome3.file-roller
    gnome3.nautilus
    inkscape
    libreoffice
    mpv
    qbittorrent
    sqlitebrowser
    xournalpp
    zynaddsubfx

    # Remote Desktop
    remmina

    # Theme
    arc-theme
    arc-icon-theme
    gnome3.adwaita-icon-theme
    lxappearance

    # Other
    gnome3.gnome-clocks
    gnucash
  ];

  home.sessionVariables = {
    # Improve appearance of Java applications
    # https://wiki.archlinux.org/index.php/Java#Tips_and_tricks
    _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel";

    # Use Wayland for Chrome & Electron apps
    NIXOS_OZONE_WL = 1;
  };

  xdg.mimeApps.enable = true;
}

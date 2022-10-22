{ pkgs, ... }:

{
  imports = [
    ../package/emacs
    ../package/eog
    ../package/evince
    ../package/firefox
    ../package/gtk
    ../package/i3
    ../package/keepassxc
    ../package/lmms
    ../package/mpv
    ../package/newsflash
    ../package/speedcrunch
    ../package/sway
  ];

  home.packages = with pkgs; [
    # Administration
    pavucontrol

    # Web
    ungoogled-chromium

    # Media & Documents
    audacity
    gimp
    gnome.file-roller
    gnome.nautilus
    inkscape
    libreoffice
    qbittorrent
    sqlitebrowser
    xournalpp
    zynaddsubfx

    # Theme
    arc-theme
    arc-icon-theme
    gnome.adwaita-icon-theme

    # Other
    gnome.gnome-clocks
    gnome.pomodoro
    gnucash
  ];

  home.sessionVariables = {
    # Improve appearance of Java applications
    # https://wiki.archlinux.org/index.php/Java#Tips_and_tricks
    _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel";

    # Use Wayland for Chrome & Electron apps
    NIXOS_OZONE_WL = 1;
  };

  xdg = {
    mimeApps.enable = true;

    # mimeapps.list often gets overwritten by applications adding mimetype associations
    configFile."mimeapps.list".force = true;
  };
}

{ config, pkgs, options, ... }:

{
  imports = [
    ../../../../i3/.config/i3/home.nix
    ../../../../sway/.config/sway/home.nix
  ];

  # Packages
  home.packages = with pkgs; [
    # Web
    chromium
    firefox
    liferea

    # Media & Documents
    (audacity.override{
      wxGTK30 = wxGTK30.override {
        # Build with GTK3 for HiDPI scaling
        withGtk2 = false;
      };
    })
    blender
    evince
    gimp
    gnome3.eog
    gnome3.file-roller
    gnome3.nautilus
    inkscape
    libreoffice
    lmms
    mpv
    qbittorrent

    # Remote Desktop
    remmina

    # Theme
    arc-theme
    arc-icon-theme
    gnome3.adwaita-icon-theme
    lxappearance-gtk3

    # Other
    gnucash
    keepassxc
    pavucontrol
    speedcrunch
  ];

  # Environment
  pam.sessionVariables = {
    # Use GTK 3 settings in Qt 5
    # https://wiki.archlinux.org/index.php/Uniform_look_for_Qt_and_GTK_applications
    QT_QPA_PLATFORMTHEME = "gtk3";

    # Improve appearance of Java applications
    # https://wiki.archlinux.org/index.php/Java#Tips_and_tricks
    _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel";

    # Touchscreen support on Firefox
    MOZ_USE_XINPUT2 = "1";
  };
}

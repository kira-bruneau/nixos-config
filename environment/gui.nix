{ config, pkgs, ... }:

{
  # Services
  services.xserver = {
    enable = true;
    useGlamor = true;
    displayManager.sddm.enable = true;
    windowManager.i3 = {
      enable = true;
      extraPackages = [];
    };
  };

  # Packages
  environment.systemPackages = with pkgs; [
    # Web
    chromium
    firefox

    # Media & Documents
    audacity
    blender
    evince
    gimp
    gnome3.file-roller
    inkscape
    libreoffice
    lmms
    mpv
    qbittorrent

    # Utility
    keepassxc
    pavucontrol
    speedcrunch
  ];

  environment.variables = {
    # Use GTK 3 settings in Qt 5
    # https://wiki.archlinux.org/index.php/Uniform_look_for_Qt_and_GTK_applications
    QT_QPA_PLATFORMTHEME = "gtk3";

    # Improve appearance of Java applications
    # https://wiki.archlinux.org/index.php/Java#Tips_and_tricks
    _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel";

    # Touchscreen support on Firefox
    MOZ_USE_XINPUT2 = "1";
  };

  fonts.fontconfig.allowBitmaps = false;
}

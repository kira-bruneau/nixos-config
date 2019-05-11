{ config, pkgs, ... }:

{
  # Services
  services.xserver = {
    enable = true;
    useGlamor = true;
    displayManager.sddm.enable = true;

    # Disable xterm
    desktopManager = {
      xterm.enable = false;
      default = "none";
    };

    # Enable i3-gaps
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      extraPackages = [];
    };
  };

  programs.sway = {
    enable = true;
    extraPackages = with pkgs; [ swaylock swayidle xwayland ];
  };

  # Enable DConf
  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ gnome3.dconf ];

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

    # Theme
    arc-theme
    arc-icon-theme
    gnome3.adwaita-icon-theme
    lxappearance-gtk3
  ];

  # Environment
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

  # Disable bitmap fonts
  fonts.fontconfig.allowBitmaps = false;
}

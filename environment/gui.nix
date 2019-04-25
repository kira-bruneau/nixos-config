{ config, pkgs, ... }:

{
  # Services
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    windowManager.i3.enable = true;
  };

  # Packages
  environment.systemPackages = with pkgs; [
    chromium
    firefox
    keepassxc
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
}

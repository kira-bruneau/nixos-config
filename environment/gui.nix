{ pkgs, ... }:

{
  imports = [
    # Window managers
    ../package/sway
    #../package/i3

    # Web
    ../package/firefox
    ../package/newsflash

    # Media & Documents
    ../package/emacs
    ../package/eog
    ../package/evince
    ../package/keepassxc
    ../package/lmms
    ../package/mpv

    # Themes
    ../package/gtk

    # Utils
    ../package/speedcrunch
    ../package/gnome-pomodoro
  ];

  home.packages = with pkgs; [
    # Administration
    gnome.dconf-editor
    pavucontrol

    # Web
    jellyfin-media-player
    qbittorrent
    ungoogled-chromium

    # Chat
    discord

    # Media & Documents
    anytype
    audacity
    gimp
    gnome.file-roller
    gnome.nautilus
    gnucash
    inkscape
    libreoffice
    sqlitebrowser
    xournalpp
    zynaddsubfx

    # Utils
    gnome.gnome-clocks
    libnotify
    yabridge
    yabridgectl
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

  services.gpg-agent.pinentryFlavor = "gnome3";

  services.easyeffects.enable = true;
}

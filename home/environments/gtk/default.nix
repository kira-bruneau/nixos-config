{ config, pkgs, ... }:

{
  gtk = {
    enable = true;

    theme = {
      package = pkgs.arc-theme;
      name = "Arc";
    };

    iconTheme = {
      package = pkgs.gnome.adwaita-icon-theme;
      name = "Adwaita";
    };

    cursorTheme = {
      inherit (config.home.pointerCursor) package name size;
    };

    font.name = "sans-serif";
  };

  home = {
    sessionVariables = {
      # Use GTK 3 settings in Qt 5
      # https://wiki.archlinux.org/index.php/Uniform_look_for_Qt_and_GTK_applications
      QT_QPA_PLATFORMTHEME = "gtk3";
    };
  };
}

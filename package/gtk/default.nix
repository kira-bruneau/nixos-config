{ config, pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      arc-theme
      arc-icon-theme
      gnome.adwaita-icon-theme
    ];

    sessionVariables = {
      # Use GTK 3 settings in Qt 5
      # https://wiki.archlinux.org/index.php/Uniform_look_for_Qt_and_GTK_applications
      QT_QPA_PLATFORMTHEME = "gtk3";
    };

    # Manage GTK2 config outside of home-manager while keeping track of the files in this git repo
    file = {
      ".gtkrc-2.0".source = config.lib.file.mkOutOfStoreSymlink
        "${config.home.configDirectory}/package/gtk/gtkrc-2.0";

      ".icons/default/index.theme".source = config.lib.file.mkOutOfStoreSymlink
        "${config.home.configDirectory}/package/gtk/icon.theme";
    };
  };

  # Manage GTK3 config outside of home-manager while keeping track of the files in this git repo
  xdg.configFile = {
    "gtk-3.0".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.configDirectory}/package/gtk/gtk-3.0";
  };
}

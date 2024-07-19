{ config, pkgs, ... }:

{
  gtk = {
    enable = true;

    theme = {
      package = pkgs.arc-theme;
      name = "Arc";
    };

    iconTheme = {
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
    };

    cursorTheme = {
      inherit (config.home.pointerCursor) package name size;
    };

    font = {
      name = "sans-serif";
      size = 10;
    };
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.packages = lib.optional config.wayland.windowManager.sway.enable pkgs.iwgtk;
  programs.waybar.settings.mainBar.network.on-click = "${pkgs.iwgtk}/bin/iwgtk";
}

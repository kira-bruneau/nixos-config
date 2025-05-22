{
  config,
  lib,
  pkgs,
  ...
}:

lib.mkIf (config.programs ? niri) {
  home.packages = [ pkgs.iwgtk ];
  programs.waybar.settings.mainBar.network.on-click = "${lib.getExe pkgs.iwgtk}";
}

{ pkgs, ... }:

{
  home.packages = [ pkgs.iwgtk ];
  programs.waybar.settings.mainBar.network.on-click = "${pkgs.iwgtk}/bin/iwgtk";
}

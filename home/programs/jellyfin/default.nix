{ pkgs, ... }:

{
  home.packages = with pkgs; [ jellyfin-media-player ];
  wayland.windowManager.sway.config.assigns."4" = [ { app_id = "^org.jellyfin.$"; } ];
  xdg.dataFile."jellyfinmediaplayer/scripts/mpris.so".source = "${pkgs.mpvScripts.mpris}/share/mpv/scripts/mpris.so";
}

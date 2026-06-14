{ pkgs, ... }:

{
  home.packages = with pkgs; [ jellyfin-desktop ];
  wayland.windowManager.sway.config.assigns."4" = [ { app_id = "^org.jellyfin.JellyfinDesktop$"; } ];
  xdg.dataFile."jellyfin-desktop/scripts/mpris.so".source =
    "${pkgs.mpvScripts.mpris}/share/mpv/scripts/mpris.so";
}

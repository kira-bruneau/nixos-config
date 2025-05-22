{ pkgs, ... }:

{
  home.packages = with pkgs; [ jellyfin-media-player ];

  programs.niri.settings = {
    window-rules = [
      {
        matches = [ { app-id = "^com.github.iwalton3.jellyfin-media-player$"; } ];
        open-on-workspace = "1-browsing";
        open-fullscreen = true;
      }
    ];
  };

  xdg.dataFile."jellyfinmediaplayer/scripts/mpris.so".source =
    "${pkgs.mpvScripts.mpris}/share/mpv/scripts/mpris.so";
}

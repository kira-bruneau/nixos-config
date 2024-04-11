{ pkgs, ... }:

{
  home.packages = with pkgs; [
    jellyfin-media-player
  ];

  wayland.windowManager.sway.config.window.commands = [
    {
      criteria = { app_id = "^org.jellyfin.$"; };
      command = "move container to workspace 4";
    }
  ];

  xdg.dataFile."jellyfinmediaplayer/scripts/mpris.so".source =
    "${pkgs.mpvScripts.mpris}/share/mpv/scripts/mpris.so";
}

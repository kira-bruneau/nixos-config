{ pkgs, ... }:

{
  home.packages = with pkgs; [
    jellyfin-media-player
  ];

  wayland.windowManager.sway.config = {
    assigns."4" = [{ app_id = "^org.jellyfin.$"; }];
    window.commands = [
      {
        criteria = { app_id = "^org.jellyfin.$"; };
        command = "inhibit_idle fullscreen";
      }
    ];
  };

  xdg.dataFile."jellyfinmediaplayer/scripts/mpris.so".source =
    "${pkgs.mpvScripts.mpris}/share/mpv/scripts/mpris.so";
}

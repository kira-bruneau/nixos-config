{ pkgs, ... }:

{
  home.packages = with pkgs; [
    ffmpeg
    gimp
    inkscape
  ];

  wayland.windowManager.sway.config = {
    assigns."1" = [ { app_id = "^chromium-browser$"; } ];
    assigns."4" = [
      { app_id = "^org.inkscape.Inkscape$"; }
      { class = "^Gimp"; }
    ];
  };
}

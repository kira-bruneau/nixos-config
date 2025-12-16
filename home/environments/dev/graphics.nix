{ pkgs, ... }:

{
  home.packages = with pkgs; [
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

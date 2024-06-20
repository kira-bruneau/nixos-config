{ pkgs, ... }:

let
  element-desktop = pkgs.element-desktop;
in
{
  home.packages = [ element-desktop ];

  wayland.windowManager.sway.config = {
    startup = [ { command = "${element-desktop}/bin/element-desktop"; } ];
    assigns."10" = [ { app_id = "^Element$"; } ];
  };
}

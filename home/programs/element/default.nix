{ pkgs, ... }:

let
  element-desktop = pkgs.element-desktop.override { electron = pkgs.electron_29; };
in
{
  home.packages = [ element-desktop ];

  wayland.windowManager.sway.config = {
    startup = [ { command = "${element-desktop}/bin/element-desktop"; } ];
    assigns."10" = [ { app_id = "^Element$"; } ];
  };
}

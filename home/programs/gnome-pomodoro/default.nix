{ config, lib, ... }:

let
  exe = lib.getExe config.programs.gnome-pomodoro.package;
in
{
  programs.gnome-pomodoro = {
    enable = true;
    settings = {
      enabled-plugins = [
        "dark-theme"
        "sounds"
      ];
    };
  };

  wayland.windowManager.sway.config =
    let
      cfg = config.wayland.windowManager.sway.config;
    in
    {
      keybindings = lib.mkOptionDefault {
        "${cfg.modifier}+p" = "exec ${exe} --start-stop";
        "${cfg.modifier}+Shift+p" = "exec ${exe} --pause-resume";
      };

      startup = [ { command = "${exe} --no-default-window --stop"; } ];
    };
}

{ config, pkgs, ... }:

let
  package = config.services.mako.package;
  makoctl = "${package}/bin/makoctl";
  grep = "${pkgs.gnugrep}/bin/grep";
in
{
  services.mako = {
    enable = true;
    settings = {
      default-timeout = 10000;
      group-by = "app-name,summary";
      anchor = "bottom-right";
      padding = "20";
      margin = "0";
      width = 320;
      border-size = 0;
      background-color = "#2b303b99";
      font = "sans-serif 10";
      text-color = "#eeeeee";
      outer-margin = 20;
      "mode=do-not-disturb" = {
        invisible = 1;
        ignore-timeout = true;
        default-timeout = 0;
      };
    };
  };

  wayland.windowManager.sway.extraConfig = ''
    layer_effects 'notifications' 'blur enable; corner_radius 10; shadows enable'
  '';

  services.swayidle.timeouts = [
    {
      timeout = 30;
      command = "${makoctl} mode -a sticky";
      resumeCommand = toString (
        pkgs.writeShellScript "resume-notification-timeout" ''
          if ! ${makoctl} mode | ${grep} -q invisible; then
            ${makoctl} mode -r sticky
          fi
        ''
      );
    }
  ];

  programs.gnome-pomodoro-swayidle = {
    onstart = [ "${makoctl} mode -a do-not-disturb" ];
    onend = [ "${makoctl} mode -r do-not-disturb" ];
  };
}

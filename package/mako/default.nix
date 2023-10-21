{ lib, config, pkgs, ... }:

let
  package = config.services.mako.package;
  makoctl = "${package}/bin/makoctl";
  grep = "${pkgs.gnugrep}/bin/grep";
in
{
  services.mako = {
    enable = true;

    defaultTimeout = 10000;
    groupBy = "app-name,summary";

    anchor = "bottom-right";
    padding = "20";
    margin = "0";
    width = 320;
    borderSize = 0;
    backgroundColor = "#2b303b99";
    font = "sans-serif 10";
    textColor = "#eeeeee";

    extraConfig = ''
      outer-margin=20

      [mode=invisible]
      invisible=1

      [mode=sticky]
      ignore-timeout=true
      default-timeout=0
    '';
  };

  wayland.windowManager.sway.extraConfig = ''
    layer_effects 'notifications' 'blur enable; corner_radius 10; shadows enable'
  '';

  services.swayidle.timeouts = [
    {
      timeout = 30;
      command = "${makoctl} mode -a sticky";
      resumeCommand = toString (pkgs.writeShellScript "resume-notification-timeout" ''
        if ! ${makoctl} mode | ${grep} -q invisible; then
          ${makoctl} mode -r sticky
        fi
      '');
    }
  ];
}

{ lib, config, pkgs, ... }:

{
  services.mako = {
    enable = true;

    defaultTimeout = 10000;

    anchor = "bottom-right";
    padding = "20";
    margin = "0";
    width = 320;
    borderSize = 0;
    backgroundColor = "#2b303b99";
    font = "Iosevka Nerd Font 10";
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
}

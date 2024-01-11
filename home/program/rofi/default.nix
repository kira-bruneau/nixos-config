{ lib, config, pkgs, ... }:

let
  rofi = pkgs.rofi-wayland;
  rofimoji = pkgs.rofimoji.override { inherit rofi; };
in
{
  imports = [
    ./themes.nix
  ];

  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
  };

  home.packages = with pkgs; [
    rofimoji
  ];

  wayland.windowManager.sway = {
    config =
      let
        cfg = config.wayland.windowManager.sway.config;
      in
      {
        menu = "${rofi}/bin/rofi -show drun -theme icon-grid -matching fuzzy";
        keybindings = lib.mkOptionDefault {
          "${cfg.modifier}+x" = "exec ${rofi}/bin/rofi -show run -matching fuzzy";
          "${cfg.modifier}+w" = "exec ${rofi}/bin/rofi -show window -matching fuzzy";
          "${cfg.modifier}+c" = "exec ${rofi}/bin/rofi -show ssh -matching fuzzy";
          "${cfg.modifier}+m" = "exec ${rofimoji}/bin/rofimoji";
        };
      };

    extraConfig = ''
      layer_effects 'rofi' 'blur enable'
    '';
  };
}

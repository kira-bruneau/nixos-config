{ lib, config, pkgs, ... }:

let
  rofimoji = pkgs.rofimoji.override {
    rofi = config.programs.rofi.package;
  };
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
          menu = "${config.programs.rofi.package}/bin/rofi -show drun -theme icon-grid -matching fuzzy";
          keybindings = lib.mkOptionDefault {
            "${cfg.modifier}+x" = "exec ${config.programs.rofi.package}/bin/rofi -show run -matching fuzzy";
            "${cfg.modifier}+w" = "exec ${config.programs.rofi.package}/bin/rofi -show window -matching fuzzy";
            "${cfg.modifier}+c" = "exec ${config.programs.rofi.package}/bin/rofi -show ssh -matching fuzzy";
            "${cfg.modifier}+m" = "exec ${rofimoji}/bin/rofimoji";
          };
        };

    extraConfig = ''
      layer_effects 'rofi' 'blur enable'
    '';
  };
}

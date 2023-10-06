{ lib, config, pkgs, ... }:

let
  rofimoji = pkgs.rofimoji.override {
    rofi = config.programs.rofi.package;
  };
in
{
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
  };

  home.packages = with pkgs; [
    rofimoji
  ];

  # TODO: Generate configuration from Nix
  xdg.configFile.rofi.source = ./config;

  wayland.windowManager.sway.config =
    let
      cfg = config.wayland.windowManager.sway.config;
    in
    {
      menu = "${config.programs.rofi.package}/bin/rofi -show drun -matching fuzzy";
      keybindings = lib.mkOptionDefault {
        "${cfg.modifier}+x" = "exec ${config.programs.rofi.package}/bin/rofi -show run -matching fuzzy";
        "${cfg.modifier}+w" = "exec ${config.programs.rofi.package}/bin/rofi -show window -matching fuzzy";
        "${cfg.modifier}+c" = "exec ${config.programs.rofi.package}/bin/rofi -show ssh -matching fuzzy";
        "${cfg.modifier}+m" = "exec ${rofimoji}/bin/rofimoji";
      };
    };
}

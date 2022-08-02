{ pkgs, ... }:

let
  i3-autoscale = pkgs.writers.writePython3Bin "i3-autoscale" {} ./bin/i3-autoscale.py;
in
{
  imports = [
    ../alacritty
    ../picom
    ../polybar
    ../rofi
  ];

  # TODO: Use Home Manager to manage X session
  # xsession = {
  #   enable = true;
  #   windowManager.i3 = {
  #     enable = true;
  #   };
  # };

  home = {
    packages = with pkgs; [
      brightnessctl
      dunst # TODO: Run as service
      feh # TODO: Use services.random-background instead
      font-awesome_5
      i3-autoscale
      pavucontrol
      python3
      scrot
      sound-theme-freedesktop
      xorg.xdpyinfo
    ];

    file = {
      ".Xresources".source = ./.Xresources;
    };
  };

  # TODO: Generate configuration from Nix
  xdg.configFile.i3.source = ./config;
}

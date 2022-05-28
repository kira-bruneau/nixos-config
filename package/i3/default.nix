{ pkgs, ... }:

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
      pavucontrol
      python3
      scrot
      sound-theme-freedesktop
      xorg.xdpyinfo
    ];

    file = {
      ".Xresources".source = ./.Xresources;
      "bin/i3-autoscale".source = ./bin/i3-autoscale;
    };
  };

  # TODO: Generate configuration from Nix
  xdg.configFile.i3.source = ./config;
}

{ config, pkgs, ... }:

{
  imports = [
    ../../../alacritty/.config/alacritty/home.nix
  ];

  # TODO: Use Home Manager to manage X session
  # xsession = {
  #   enable = true;
  #   windowManager.i3 = {
  #     enable = true;
  #   };
  # };

  home.packages = with pkgs; with nur.repos.metadark; [
    dunst # TODO: Run as service
    feh # TODO: Use services.random-background instead
    font-awesome_5
    brightnessctl
    pavucontrol
    polybarFull # TODO: Run as service
    python3
    rofi-wayland
    scrot
    sound-theme-freedesktop
    xorg.xdpyinfo
  ];

  services.picom = {
    enable = true;
    backend = "glx";
    shadow = true;
    shadowExclude = [
      "class_i = 'i3-frame'" # title bars
      "name = 'Polybar tray window'" # polybar tray
      "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'" # background windows in tabbed layout
    ];
    opacityRule = [
      "90:name = 'Picture-in-Picture'"
    ];
    fade = true;
    fadeDelta = 10; # 100 steps per second
    fadeSteps = [ "0.0666" "0.0444" ]; # ~150ms ~225ms
    extraOptions = ''
      clear-shadow = true;
      blur-background = true;
      no-fading-destroyed-argb = true;
    '';
  };
}

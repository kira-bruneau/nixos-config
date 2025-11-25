{ lib, pkgs, ... }:

let
  valent = pkgs.valent;
in
{
  home.packages = [ valent ];
  wayland.windowManager.sway.config.startup = [
    { command = "${lib.getExe valent} --gapplication-service"; }
  ];
}

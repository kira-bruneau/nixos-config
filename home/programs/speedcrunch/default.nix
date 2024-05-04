{ config, pkgs, ... }:

let
  settingsFormat = pkgs.formats.ini { };
in
{
  imports = [
    ../../environments/config.nix
  ];

  home.packages = with pkgs; [
    speedcrunch
  ];

  xdg = {
    configFile."SpeedCrunch/SpeedCrunch.ini".source = settingsFormat.generate "SpeedCrunch.ini" {
      General.ConfigVersion = 1200;
      SpeedCrunch."Display\\ColorSchemeName" = "Tomorrow Night";
    };
  };
}

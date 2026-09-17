{ lib, pkgs, ... }:

let
  settingsFormat = pkgs.formats.ini { };
in
{
  home.packages = with pkgs; [ speedcrunch ];

  xdg = {
    configFile."SpeedCrunch/SpeedCrunch.ini".source = settingsFormat.generate "SpeedCrunch.ini" {
      General.ConfigVersion = 1200;
      SpeedCrunch."Display\\ColorSchemeName" = "Tomorrow Night";
    };
  };

  dconf.settings = {
    "org/gnome/settings-daemon/plugins/media-keys" = {
      custom-keybindings = [
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/calculator/"
      ];
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/calculator" = {
      name = "Open Calculator";
      command = lib.getExe pkgs.speedcrunch;
      binding = "<Super>c";
    };
  };
}

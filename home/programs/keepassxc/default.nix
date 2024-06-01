{ config, pkgs, ... }:

let
  settingsFormat = pkgs.formats.ini { };
in
{
  imports = [ ../../environments/config.nix ];

  home.packages = with pkgs; [ keepassxc ];

  xdg.configFile."keepassxc/keepassxc.ini".source = settingsFormat.generate "keepassxc.ini" {
    General.ConfigVersion = 2;
    Browser.Enabled = true;

    GUI = {
      ApplicationTheme = "dark";
      MinimizeOnClose = true;
      MinimizeOnStartup = true;
      MinimizeToTray = true;
      ShowTrayIcon = true;
    };

    PasswordGenerator = {
      Length = 20;
      SpecialChars = true;
    };

    Security = {
      IconDownloadFallback = true;
      LockDatabaseIdle = true;
    };
  };

  wayland.windowManager.sway.config.startup = [ { command = "${pkgs.keepassxc}/bin/keepassxc"; } ];
}

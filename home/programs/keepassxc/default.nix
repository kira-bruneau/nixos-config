{ config, pkgs, ... }:

let
  settingsFormat = pkgs.formats.ini { };
  keepassxc = pkgs.keepassxc;
in
{
  imports = [ ../../environments/config.nix ];

  home.packages = [ keepassxc ];

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

  wayland.windowManager.sway.config.startup = [ { command = "${keepassxc}/bin/keepassxc"; } ];

  # Firefox integration
  programs.firefox.policies.ExtensionSettings."keepassxc-browser@keepassxc.org" = {
    installation_mode = "force_installed";
    install_url = "https://addons.mozilla.org/firefox/downloads/latest/keepassxc-browser/latest.xpi";
  };
}

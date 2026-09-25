{
  config,
  lib,
  ...
}:

{
  programs.keepassxc = {
    enable = true;
    autostart = config.xdg.autostart.enable;
    settings = {
      General.ConfigVersion = 2;

      Browser = {
        Enabled = true;
        UpdateBinaryPath = false;
      };

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
  };

  wayland.windowManager.sway.config.startup = [
    { command = lib.getExe config.programs.keepassxc.package; }
  ];

  # Librewolf integration
  programs.librewolf = {
    policies.ExtensionSettings."keepassxc-browser@keepassxc.org" = {
      installation_mode = "force_installed";
      install_url = "https://addons.mozilla.org/firefox/downloads/latest/keepassxc-browser/latest.xpi";
    };

    nativeMessagingHosts = [ config.programs.keepassxc.package ];
  };
}

{ config, lib, ... }:

{
  services.getty = {
    autologinUser = config.users.defaultUser;
    autologinOnce = true;
  };

  services.greetd.settings.initial_session = lib.mkIf config.programs.sway.enable {
    command = "sway";
    user = config.users.defaultUser;
  };

  services.displayManager.autoLogin = {
    enable = true;
    user = config.users.defaultUser;
  };

  programs.dconf.profiles = lib.mkIf config.services.desktopManager.gnome.enable {
    user.databases = [
      {
        settings = {
          "org/gnome/desktop/screensaver" = {
            lock-enabled = false;
          };
        };
      }
    ];
  };
}

{ config, lib, ... }:

{
  services.getty.autologinUser = config.users.defaultUser;

  services.greetd.settings.initial_session = {
    command = "sway";
    user = config.users.defaultUser;
  };

  services.displayManager.autoLogin = {
    enable = true;
    user = config.users.defaultUser;
  };

  programs.dconf.profiles = lib.mkIf config.services.xserver.desktopManager.gnome.enable {
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

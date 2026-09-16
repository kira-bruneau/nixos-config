{ config, lib, ... }:

{
  services.getty = {
    autologinUser = config.users.defaultUser;
    autologinOnce = true;
  };

  security.pam.services.getty = {
    enableGnomeKeyring = true;
  };

  services.greetd.settings.initial_session = lib.mkIf config.programs.sway.enable {
    command = "sway";
    user = config.users.defaultUser;
  };

  services.displayManager.autoLogin = {
    enable = true;
    user = config.users.defaultUser;
  };
}

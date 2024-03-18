{ config, lib, ... }:

{
  services.getty.autologinUser = config.users.defaultUser;

  services.greetd.settings.initial_session = {
    command = "sway";
    user = config.users.defaultUser;
  };

  services.xserver.displayManager.autoLogin = {
    enable = true;
    user = config.users.defaultUser;
  };
}
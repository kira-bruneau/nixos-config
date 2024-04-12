{ lib, ... }:

{
  users.users.kira = {
    password = "kira";
    hashedPasswordFile = lib.mkForce null;
  };

  services.greetd.settings.initial_session = {
    command = "sway";
    user = "kira";
  };

  services.xserver.displayManager.autoLogin = {
    enable = true;
    user = "kira";
  };
}

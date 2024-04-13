{
  services.getty.autologinUser = "kira";

  services.greetd.settings.initial_session = {
    command = "sway";
    user = "kira";
  };

  services.xserver.displayManager.autoLogin = {
    enable = true;
    user = "kira";
  };
}

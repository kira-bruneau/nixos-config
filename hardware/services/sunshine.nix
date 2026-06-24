{
  services.sunshine = {
    enable = true;
    autoStart = true;
    openFirewall = true;
  };

  # Sunshine enables avahi by default, I don't need it
  services.avahi.enable = false;
}

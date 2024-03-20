{ config, ... }:

{
  location.provider = "geoclue2";

  # Disable unused geoclue sources
  services.geoclue2 = {
    # WiFi
    enableWifi = true;

    # GPS (requires Avahi)
    enableNmea = false;

    # Cellular (requires ModemManager)
    enable3G = false;
    enableCDMA = false;
    enableModemGPS = false;
  };

  systemd.services.geoclue.wants = [ "network-online.target" ];

  systemd.user.services.geoclue-agent.wants = [ "network-online.target" ];

  environment.etc."geolocation".text = ''
    ${toString config.location.latitude}
    ${toString config.location.longitude}
    -1.7976931348623157e+308
    -1
  '';

  services.localtimed.enable = true;
}

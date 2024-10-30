{ config, ... }:

{
  imports = [
    ../services/cec-sync.nix
  ];

  location.provider = "geoclue2";

  # Disable unused geoclue sources
  services.geoclue2 = {
    # WiFi
    enableWifi = true;
    geoProviderUrl = "https://api.beacondb.net/v1/geolocate";

    # GPS (requires Avahi)
    enableNmea = false;

    # Cellular (requires ModemManager)
    enable3G = false;
    enableCDMA = false;
    enableModemGPS = false;
  };

  environment.etc."geolocation".text = ''
    ${toString config.location.latitude}
    ${toString config.location.longitude}
    -1.7976931348623157e+308
    -1
  '';

  services.automatic-timezoned.enable = true;
  time.timeZone = null;
}

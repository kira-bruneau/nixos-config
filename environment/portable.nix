{ ... }:

{
  imports = [
    ./wifi.nix
    ./bluetooth.nix
  ];

  location.provider = "geoclue2";

  # Disable unused geoclue sources
  services.geoclue2 = {
    # WiFi (requires wpa_supplicant)
    # Failed to query location: No WiFi networks found
    enableWifi = false;

    # GPS (requires Avahi)
    enableNmea = false;

    # Cellular (requires ModemManager)
    enable3G = false;
    enableCDMA = false;
    enableModemGPS = false;
  };

  services.localtimed.enable = true;
}

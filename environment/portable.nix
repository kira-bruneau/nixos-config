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

  services.localtimed.enable = true;
}

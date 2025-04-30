{ config, lib, ... }:

{
  imports = [
    ../services/cec-sync.nix
  ];

  location.provider = "geoclue2";

  services.geoclue2 = {
    enableStatic = true;

    # WiFi
    enableWifi = lib.mkForce true;
    geoProviderUrl = "https://api.beacondb.net/v1/geolocate";
  };

  services.automatic-timezoned.enable = true;
}

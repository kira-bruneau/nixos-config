{ lib, ... }:

lib.mkDefault {
  location = {
    latitude = 45.424721;
    longitude = -75.695;
  };

  services.geoclue2 = {
    staticAltitude = 100;
    staticAccuracy = 50000;
  };

  time.timeZone = "Canada/Eastern";
}

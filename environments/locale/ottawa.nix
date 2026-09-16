{ lib, ... }:

{
  location = {
    latitude = 45.424721;
    longitude = -75.695;
  };

  services.geoclue2 = {
    staticAltitude = 100;
    staticAccuracy = 50000;
  };

  # automatic-timezoned annoyingly sets this to null by default
  time.timeZone = lib.mkOverride 99 "Canada/Eastern";
}

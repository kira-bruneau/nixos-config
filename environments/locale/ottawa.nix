{ lib, ... }:

lib.mkDefault {
  location = {
    latitude = 45.424721;
    longitude = -75.695;
  };

  time.timeZone = "Canada/Eastern";
}

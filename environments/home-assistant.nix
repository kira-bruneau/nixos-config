{ config, ... }:

{
  services.home-assistant = {
    enable = true;
    extraComponents = [ "vesync" ];
    extraPackages = ps: with ps; [ numpy ];
    config.homeassistant = {
      name = "Home";
      latitude = config.location.latitude;
      longitude = config.location.longitude;
      unit_system = "metric";
    };
  };
}

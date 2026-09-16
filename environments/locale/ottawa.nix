{ config, lib, ... }:

let
  degreesToRadians = x: x * 3.1415927 / 180;

  mkCoordinates =
    latitude: longitude:
    (lib.gvariant.mkTuple [
      (degreesToRadians latitude)
      (degreesToRadians longitude)
    ]);

  mkWeatherLocation =
    {
      name,
      default,
      latitude,
      longitude,
      station,
    }:
    with lib.gvariant;
    (mkVariant (mkTuple [
      (mkUint32 2) # version
      (mkVariant (mkTuple [
        name
        station.name
        default
        [ (mkCoordinates station.latitude station.longitude) ]
        [ (mkCoordinates latitude longitude) ]
      ]))
    ]));
in
{
  location = {
    latitude = 45.424721;
    longitude = -75.695;
  };

  services.geoclue2 = {
    staticAltitude = 100;
    staticAccuracy = 50000;
  };

  programs.dconf.profiles.user.databases = [
    (lib.mkIf config.services.desktopManager.gnome.enable (
      let
        weatherLocations = map mkWeatherLocation [
          {
            name = "Ottawa";
            default = true;
            inherit (config.location) latitude longitude;

            station = {
              name = "CYOW";
              latitude = 45.316667;
              longitude = -75.666667;
            };
          }
        ];
      in
      {
        settings = {
          "org/gnome/shell/weather" = {
            automatic-location = false;
            locations = weatherLocations;
          };

          "org/gnome/Weather".locations = weatherLocations;
        };
      }
    ))
  ];

  # automatic-timezoned annoyingly sets this to null by default
  time.timeZone = lib.mkOverride 99 "Canada/Eastern";
}

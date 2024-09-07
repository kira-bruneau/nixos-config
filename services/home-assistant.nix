{ config, pkgs, ... }:

let
  waves = pkgs.fetchFromGitHub {
    owner = "tgcowell";
    repo = "waves";
    rev = "refs/tags/v2.4.1";
    hash = "sha256-koUOdwt9t6iOQn4T/voV4Hvc5moqnf/PkXFp/nuFytM=";
  };
in
{
  services.home-assistant = {
    enable = true;
    extraComponents = [ "vesync" ];
    extraPackages = ps: with ps; [ numpy ];
    config = {
      homeassistant = {
        name = "Home";
        latitude = config.location.latitude;
        longitude = config.location.longitude;
        unit_system = "metric";
      };

      frontend = {
        themes = "!include ${waves}/themes/waves.yaml";
      };
    };
  };

  systemd.services.home-assistant.preStart = ''
    mkdir -p ${config.services.home-assistant.configDir}/www
    ln -fns ${waves}/themes ${config.services.home-assistant.configDir}/www/waves
  '';
}

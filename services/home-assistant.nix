{ config, pkgs, ... }:

let
  waves = pkgs.fetchFromGitHub {
    owner = "tgcowell";
    repo = "waves";
    rev = "refs/tags/v2.4.1";
    hash = "sha256-JWMUf6WNBmFcV9HjdHLsmeLLm+5VqxcdxGDsmtpLnmM=";
  };
in
{
  services.home-assistant = {
    enable = true;

    extraComponents = [
      # Base components from core/homeassistant/helpers/service.py
      "ai_task"
      "assist_satellite"

      "vesync"
    ];

    config = {
      frontend.themes = "!include ${waves}/themes/waves.yaml";

      homeassistant = {
        name = "Home";
        latitude = config.location.latitude;
        longitude = config.location.longitude;
        unit_system = "metric";
      };

      http = {
        use_x_forwarded_for = true;
        trusted_proxies = [ "127.0.0.1" ];
      };
    };
  };

  systemd.services.home-assistant.preStart = ''
    mkdir -p ${config.services.home-assistant.configDir}/www
    ln -fns ${waves}/themes ${config.services.home-assistant.configDir}/www/waves
  '';

  services.nginx.virtualHosts = {
    "home-assistant.jakira.space".locations."/" = {
      proxyPass = "http://127.0.0.1:8123";
      recommendedProxySettings = true;
      proxyWebsockets = true;
    };
  };
}

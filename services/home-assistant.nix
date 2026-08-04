{ config, pkgs, ... }:

let
  waves = pkgs.fetchFromGitHub {
    owner = "tgcowell";
    repo = "waves";
    rev = "refs/tags/v2.4.1";
    hash = "sha256-JWMUf6WNBmFcV9HjdHLsmeLLm+5VqxcdxGDsmtpLnmM=";
    passthru.isHomeAssistantTheme = true;
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

    themes = [ waves ];

    config = {
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
}

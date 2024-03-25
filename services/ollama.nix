{ pkgsUnstable, ... }:

let
  ollama = pkgsUnstable.ollama.override { acceleration = "rocm"; };
in
{
  systemd.services.ollama = {
    description = "Ollama Service";
    after = [ "network-online.target" ];
    wantedBy = [ "default.target" ];
    environment.OLLAMA_HOST = "0.0.0.0";
    script = "HOME=$STATE_DIRECTORY ${ollama}/bin/ollama serve";
    serviceConfig = {
      DynamicUser = true;
      Restart = "always";
      RestartSec = 3;
      StateDirectory = "ollama";
    };
  };
}

{
  config,
  lib,
  pkgsUnstable,
  ...
}:

{
  services.ollama = {
    enable = true;
    package = pkgsUnstable.ollama;
    host = "0.0.0.0";
    port = 11434;
    acceleration = "rocm";
    environmentVariables.OLLAMA_KEEP_ALIVE = "672h";
  };

  systemd.services.ollama-preload = {
    wantedBy = [ "ollama.service" ];
    after = [ "ollama.service" ];
    environment = config.systemd.services.ollama.environment;
    serviceConfig = {
      Type = "oneshot";
      DynamicUser = true;
      Restart = "on-failure";

      # bounded exponential backoff
      RestartSec = "1s";
      RestartMaxDelaySec = "2h";
      RestartSteps = "10";

      ExecStart = ''
        ${lib.getExe config.services.ollama.package} run qwen2.5-coder:32b-instruct-q4_K_M ""
      '';
    };
  };
}

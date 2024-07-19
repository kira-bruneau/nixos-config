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

  # Preload commonly used model
  systemd.services.ollama.postStart = ''
    ${lib.getExe config.services.ollama.package} run deepseek-coder-v2 ""
  '';
}

{ pkgsUnstable, ... }:

{
  services.ollama = {
    enable = true;
    package = pkgsUnstable.ollama;
    listenAddress = "0.0.0.0:11434";
    acceleration = "rocm";
  };
}

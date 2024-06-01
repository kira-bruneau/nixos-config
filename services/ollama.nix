{ pkgsOllama, ... }:

{
  services.ollama = {
    enable = true;
    package = pkgsOllama.ollama;
    listenAddress = "0.0.0.0:11434";
    acceleration = "rocm";
  };
}

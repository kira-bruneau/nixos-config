{
  imports = [
    ../environments/gaming.nix
    ../environments/gui/sway.nix
    ../environments/media-server.nix
    ../services/habitica.nix
    ../services/home-assistant.nix
    ../services/kubo.nix
    ../services/ollama.nix
    ../services/synapse.nix
    ../users/builder.nix
    ../users/kira.nix
  ];

  system.stateVersion = "24.05";

  users.defaultUser = "kira";
}

{ config, lib, ... }:

{
  imports = [
    ../environments/gaming.nix
    ../environments/gui/sway.nix
    ../environments/media-server.nix
    ../services/kubo.nix
    ../users/builder.nix
    ../users/kira.nix
  ];

  system.stateVersion = "24.05";

  users.defaultUser = "kira";
}

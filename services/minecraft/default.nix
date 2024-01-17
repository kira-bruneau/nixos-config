{ inputs, ... }:

{
  imports = [
    inputs.nix-minecraft.nixosModules.minecraft-servers
  ];

  services.minecraft-servers = {
    enable = true;
    eula = true;
    openFirewall = true;
    dataDir = "/var/lib/minecraft";
  };
}

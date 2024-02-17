{ inputs, pkgs, ... }:

let
  pkgsNixMinecraft = inputs.nix-minecraft.legacyPackages.${pkgs.system};

  modpack = pkgs.fetchzip {
    url = "https://mediafilez.forgecdn.net/files/5109/63/BCG%2B%20Server%20Files%201.1.13HF.zip";
    stripRoot = false;
    postFetch = ''
      cd "$out"
      rm -rf eula.txt fabric-server-launch.jar libraries server.jar start.bat start.sh
    '';

    hash = "sha256-+bJ7CWmK3N9HdwkPo6mlPOSDIwSt+P3vTO5OyHZ7nFo=";
  };

  serverVersion = "fabric-1_20_1";
  fabricVersion = "0.14.25";
in
{
  imports = [
    ./.
  ];

  services.minecraft-servers.servers."BigChadGuysPlus" = {
    enable = true;
    autoStart = false;
    package = pkgsNixMinecraft.fabricServers.${serverVersion}.override { loaderVersion = fabricVersion; };
    symlinks = {
      "mods" = "${modpack}/mods";
      "resourcepacks" = "${modpack}/resourcepacks";
    };

    serverProperties.server-port = 25564;
  };

  systemd.services.minecraft-server-BigChadGuysPlus = {
    preStart = ''
      rm -rf config
      cp -r --no-preserve=mode ${modpack}/config config
    '';
  };

  systemd.sockets.minecraft-server-BigChadGuysPlus-proxy = {
    wantedBy = [ "sockets.target" ];
    requires = [ "network.target" ];
    listenStreams = [ "25565" ];
  };

  systemd.services.minecraft-server-BigChadGuysPlus-proxy = {
    requires = [ "minecraft-server-BigChadGuysPlus.service" ];
    after = [ "minecraft-server-BigChadGuysPlus.service" ];
    serviceConfig = {
      Type = "notify";
      ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd 127.0.0.1:25564";
      PrivateTmp = true;
    };
  };

  networking.firewall = {
    allowedTCPPorts = [ 25565 ];
    allowedUDPPorts = [ 25565 ];
  };
}

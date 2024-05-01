{ inputs, pkgs, pkgsNixMinecraft, ... }:

let
  modpack = pkgs.fetchzip {
    url = "https://mediafilez.forgecdn.net/files/5250/779/BCG%2B%201.1.17%20Server%20Files.zip";
    stripRoot = false;
    postFetch = ''
      cd "$out"
      rm -rf eula.txt fabric-server-launch.jar libraries server.jar start.bat start.sh
    '';

    hash = "sha256-P4hU6EdO7K/7xarXPIL+1z2V8yge9Iyh8Q35w8WG2uw=";
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
      "resourcepacks" = "${modpack}/resourcepacks";
    };

    files = {
      "ops.json".value = [{
        name = "daxvena";
        uuid = "13290979-c320-4975-b1fe-8906f36851fe";
        level = 4;
      }];
    };

    serverProperties.server-port = 25564;
  };

  systemd.services.minecraft-server-BigChadGuysPlus = {
    preStart = ''
      rm -rf config
      cp -r --no-preserve=mode ${modpack}/config config

      rm -rf mods
      mkdir mods
      ln -s ${modpack}/mods/* mods

      ln -s ${pkgs.fetchurl {
        url = "https://mediafilez.forgecdn.net/files/5080/952/create-new-age-fabric-1.20.1-1.1.2.jar";
        hash = "sha256-egBYEdjonRJCE5NuR/XyAN8u3m3aDjyVjVtvm0vJb1o=";
      }} mods/create-new-age-fabric-1.20.1-1.1.2.jar

      ln -s ${pkgs.fetchurl {
        url = "https://mediafilez.forgecdn.net/files/5118/354/botarium-fabric-1.20.1-2.3.3.jar";
        hash = "sha256-ihAOxHzjDGXg9qGuK0Vat8vwGW+HWMN0PfkZNxexbPo=";
      }} mods/botarium-fabric-1.20.1-2.3.3.jar
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

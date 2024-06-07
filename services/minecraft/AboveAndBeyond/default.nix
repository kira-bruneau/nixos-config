{ pkgs, ... }:

let
  modpack = pkgs.fetchzip {
    url = "https://mediafilez.forgecdn.net/files/3567/576/Above%20and%20Beyond-1.3-Server.zip";
    stripRoot = false;
    postFetch = ''
      cd "$out"
      rm -rf forge-1.16.5-36.2.20-installer.jar
    '';

    hash = "sha256-z784qhcrL0JTWpjXhk63Xo0K5uCBEf3yYmDjMOEqUdk=";
  };

  forge =
    pkgs.runCommand "forge-1.16.5-36.2.34"
      {
        nativeBuildInputs = with pkgs; [
          cacert
          curl
          jre_headless
          strip-nondeterminism
        ];

        outputHashMode = "recursive";
        outputHash = "sha256-jccxyIEU6KZGOQpLi6zf5rBXzFQ76mXdb9+cLTNLkVo=";
      }
      ''
        mkdir -p "$out"
        curl https://maven.minecraftforge.net/net/minecraftforge/forge/1.16.5-36.2.34/forge-1.16.5-36.2.34-installer.jar -o ./installer.jar
        java -jar ./installer.jar --installServer "$out"
        strip-nondeterminism --type jar "$out/libraries/net/minecraft/server/1.16.5-20210115.111550/server-1.16.5-20210115.111550-srg.jar"
      '';

  minecraft-server = pkgs.writeShellScriptBin "minecraft-server" ''
    exec ${pkgs.jre8}/bin/java $@ -jar ${forge}/forge-1.16.5-36.2.34.jar nogui
  '';
in
{
  imports = [ ../. ];

  services.minecraft-servers.servers."AboveAndBeyond" = {
    enable = true;
    autoStart = false;
    package = minecraft-server;

    symlinks = {
      "kubejs" = "${modpack}/kubejs";
      "mods" = "${modpack}/mods";
      "openloader" = "${modpack}/openloader";
      "server-icon.png" = "${modpack}/server-icon.png";
      "worldshape" = "${modpack}/worldshape";
    };

    files = {
      "ops.json".value = [
        {
          name = "daxvena";
          uuid = "13290979-c320-4975-b1fe-8906f36851fe";
          level = 4;
        }
      ];
    };

    serverProperties = {
      motd = "An Above and Beyond Server";
      server-port = 25562;
      difficulty = "normal";
    };
  };

  systemd.services.minecraft-server-AboveAndBeyond = {
    preStart = ''
      rm -rf config
      cp -r --no-preserve=mode ${modpack}/config config

      rm -rf defaultconfigs
      cp -r --no-preserve=mode ${modpack}/defaultconfigs defaultconfigs
    '';
  };

  systemd.sockets.minecraft-server-AboveAndBeyond-proxy = {
    wantedBy = [ "sockets.target" ];
    requires = [ "network.target" ];
    listenStreams = [ "25563" ];
  };

  systemd.services.minecraft-server-AboveAndBeyond-proxy = {
    requires = [ "minecraft-server-AboveAndBeyond.service" ];
    after = [ "minecraft-server-AboveAndBeyond.service" ];
    serviceConfig = {
      Type = "notify";
      ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd 127.0.0.1:25562";
      PrivateTmp = true;
    };
  };

  networking.firewall = {
    allowedTCPPorts = [ 25563 ];
    allowedUDPPorts = [ 25563 ];
  };
}

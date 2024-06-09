{
  lib,
  pkgs,
  pkgsNixMinecraft,
  ...
}:

let
  modpack =
    (pkgsNixMinecraft.fetchPackwizModpack {
      url = "file://${./packwiz}/pack.toml";
      side = "both";
      packHash = "sha256-M+mRvCEhN49T6reaVivJmztifGmLVZC0nITtvuvrQ88=";
    }).addFiles
      {
        "mods/automodpack-fabric-4.0.0-beta1-1.20.1.jar" = pkgs.requireFile {
          name = "automodpack-fabric-4.0.0-beta1-1.20.1.jar";
          url = "https://github.com/Skidamek/AutoModpack/actions/runs/8317674272/artifacts/1333325852";
          hash = "sha256-DxBZXiXo3psnw8+l2F2ViMJ00pgpTfIxAIERYsUKcPs=";
        };
      };

  mcVersion = modpack.manifest.versions.minecraft;
  fabricVersion = modpack.manifest.versions.fabric;
  serverVersion = lib.replaceStrings [ "." ] [ "_" ] "fabric-${mcVersion}";
in
{
  imports = [ ../. ];

  services.minecraft-servers.servers.Jakira = {
    enable = true;
    autoStart = false;
    jvmOpts = "-Xmx3G -Xms1G";
    package = pkgsNixMinecraft.fabricServers.${serverVersion}.override {
      jre_headless = pkgs.jdk17_headless;
      loaderVersion = fabricVersion;
    };

    files = {
      "automodpack/automodpack-server.json".value = {
        modpackName = "Jakira";
        generateModpackOnStart = false;
        syncedFiles = [
          "/config/artifacts/common.json5"
          "/config/bclib/client.json"
          "/config/emi.css"
          "/config/inventoryprofilesnext/inventoryprofiles.json"
          "/config/iris.properties"
          "/config/universal-graves/config.json"
          "/config/xaerominimap.txt"
          "/mods/*.jar"
          "/options.txt"
          "/resourcepacks/*"
          "/shaderpacks/*"
        ];

        allowEditsInFiles = [
          "/config/*"
          "/options.txt"
          "/shaderpacks/*.txt"
        ];
      };

      "config/artifacts/common.json5" = {
        format = pkgs.formats.json { };
        value = {
          everlastingBeefChance = 0;
        };
      };

      "config/bclib/client.json".value = {
        version = {
          "check [default: true]" = false;
          "didShowWelcome [default: false]" = true;
        };
      };

      "config/universal-graves/config.json".value = {
        config_version = 3;
        protection = {
          non_owner_protection_time = -1;
          self_destruction_time = -1;
        };
        interactions = {
          give_death_compass = false;
        };
        placement = {
          restore_replaced_block = true;
        };
      };

      "ops.json".value = [
        {
          name = "daxvena";
          uuid = "13290979-c320-4975-b1fe-8906f36851fe";
          level = 4;
        }
      ];
    };

    symlinks = {
      "config/emi.css" = pkgs.writeText "emi.css" ''
        #general {
          search-mod-name-by-default: true;
        }
      '';

      "config/inventoryprofilesnext/inventoryprofiles.json".value = {
        GuiSettings = {
          enable_inventory_editor_button.value = false;
          enable_inventory_settings_button.value = false;
          continuous_crafting_saved_value = false;
        };
        AutoRefillSettings = {
          number_of_notifications = 1;
          visual_replace_failed_notification.value = false;
          audio_replace_failed_notification.value = false;
        };
      };

      "config/iris.properties".value = {
        shaderPack = "ComplementaryUnbound.zip";
        enableShaders = true;
      };

      "config/xaerominimap.txt" = pkgs.writeText "xaerominimap.txt" ''
        lockNorth:true
        minimapOpacity:75.0
        minimapShape:1
        module;id=xaerominimap:minimap;active=true;x=0;y=0;centered=false;fromRight=true;fromBottom=false;flippedVer=false;flippedHor=false;
      '';

      "mods" = "${modpack}/mods";

      "options.txt" = pkgs.writeText "options.txt" ''
        version:3465
        joinedFirstServer:true
        skipMultiplayerWarning:true
        tutorialStep:none
      '';

      "shaderpacks/ComplementaryUnbound.zip" = "${modpack}/shaderpacks/ComplementaryUnbound_r5.2.1.zip";

      "shaderpacks/ComplementaryUnbound.zip.txt" = {
        format = pkgs.formats.keyValue { };
        value = {
          AMBIENT_MULT = 140;
          LENSFLARE = true;
          WATER_ALPHA_MULT = 240;
          WATER_FOG_MULT = 180;
          WATER_REFRACTION_INTENSITY = 2.6;
        };
      };
    };

    serverProperties = {
      difficulty = "normal";
      enforce-secure-profile = false;
      level-seed = 4013200787929413176;
      server-port = 25564;
    };
  };

  systemd.sockets.minecraft-server-Jakira-proxy = {
    wantedBy = [ "sockets.target" ];
    requires = [ "network.target" ];
    listenStreams = [ "25565" ];
  };

  systemd.services.minecraft-server-Jakira-proxy = {
    requires = [ "minecraft-server-Jakira.service" ];
    after = [ "minecraft-server-Jakira.service" ];
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

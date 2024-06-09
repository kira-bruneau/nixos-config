{
  inputs,
  lib,
  pkgs,
  pkgsNixMinecraft,
  ...
}:

let
  inherit (inputs.packwiz2nix.lib) mkPackwizPackages mkModLinks;
  mods = mkPackwizPackages pkgs ./checksums.json;
  versions = (builtins.fromTOML (builtins.readFile ./packwiz/pack.toml)).versions;
  serverVersion = lib.replaceStrings [ "." ] [ "_" ] "fabric-${versions.minecraft}";
in
{
  imports = [ ../. ];

  services.minecraft-servers.servers.Jakira = {
    enable = true;
    autoStart = false;
    jvmOpts = "-Xmx3G -Xms1G";
    package = pkgsNixMinecraft.fabricServers.${serverVersion}.override {
      jre_headless = pkgs.jdk17_headless;
      loaderVersion = versions.fabric;
    };

    files = {
      "automodpack/automodpack-server.json".value = {
        modpackName = "Jakira";
        generateModpackOnStart = false;
        syncedFiles = [
          "/config/artifacts/common.json5"
          "/config/bclib/client.json"
          "/config/carryon-common.json"
          "/config/emi.css"
          "/config/inventoryprofilesnext/integrationHints/travelersbackpack.json"
          "/config/inventoryprofilesnext/inventoryprofiles.json"
          "/config/iris.properties"
          "/config/item_obliterator.json5"
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

      "config/carryon-common.json".value = {
        blacklist = {
          forbiddenTiles = [
            "#forge:immovable"
            "#forge:relocation_not_supported"
            "ad_astra:*"
            "ae2:*"
            "create*:*"
            "minecraft:*_bed"
            "minecraft:*_door"
            "minecraft:big_dripleaf_stem"
            "minecraft:cake"
            "minecraft:end_gateway"
            "minecraft:end_portal"
            "minecraft:large_fern"
            "minecraft:lilac"
            "minecraft:nether_portal"
            "minecraft:peony"
            "minecraft:piston_head"
            "minecraft:rose_bush"
            "minecraft:sunflower"
            "minecraft:tall_grass"
            "minecraft:tall_seagrass"
            "minecraft:waterlily"
            "tconstruct:*"
          ];
        };
      };

      "config/item_obliterator.json5" = {
        format = pkgs.formats.json { };
        value = {
          configVersion = 2;

          blacklisted_items = [
            "artifacts:everlasting_beef"
            "artifacts:eternal_steak"
          ];

          only_disable_recipes = [
            "travelersbackpack:black_sleeping_bag"
            "travelersbackpack:blue_sleeping_bag"
            "travelersbackpack:brown_sleeping_bag"
            "travelersbackpack:cyan_sleeping_bag"
            "travelersbackpack:gray_sleeping_bag"
            "travelersbackpack:green_sleeping_bag"
            "travelersbackpack:light_blue_sleeping_bag"
            "travelersbackpack:light_gray_sleeping_bag"
            "travelersbackpack:lime_sleeping_bag"
            "travelersbackpack:magenta_sleeping_bag"
            "travelersbackpack:orange_sleeping_bag"
            "travelersbackpack:pink_sleeping_bag"
            "travelersbackpack:purple_sleeping_bag"
            "travelersbackpack:red_sleeping_bag"
            "travelersbackpack:white_sleeping_bag"
            "travelersbackpack:yellow_sleeping_bag"
          ];
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

    symlinks = (mkModLinks mods) // {
      "mods/automodpack-fabric-4.0.0-beta1-1.20.1.jar" = pkgs.requireFile {
        name = "automodpack-fabric-4.0.0-beta1-1.20.1.jar";
        url = "https://github.com/Skidamek/AutoModpack/actions/runs/8317674272/artifacts/1333325852";
        hash = "sha256-DxBZXiXo3psnw8+l2F2ViMJ00pgpTfIxAIERYsUKcPs=";
      };

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

      "config/inventoryprofilesnext/integrationHints/travelersbackpack.json".value = {
        "com.tiviacz.travelersbackpack.client.screen.TravelersBackpackHandledScreen" = {
          ignore = true;
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

      "options.txt" = pkgs.writeText "options.txt" ''
        version:3465
        joinedFirstServer:true
        skipMultiplayerWarning:true
        tutorialStep:none
      '';

      # TODO: This should be included as part of packwiz2nix
      "shaderpacks/ComplementaryUnbound.zip" = pkgs.fetchurl {
        url = "https://cdn.modrinth.com/data/R6NEzAwj/versions/qWTl3wic/ComplementaryUnbound_r5.2.1.zip";
        hash = "sha256-+1BFqK4FUXId3FyPOSgFfNZBXMw8p9+UfK+1VWRUfdA=";
      };

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

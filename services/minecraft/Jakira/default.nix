{
  lib,
  pkgs,
  pkgsNixMinecraft,
  ...
}:

let
  importPackwizMetaFile =
    root: path:
    let
      meta = builtins.fromTOML (builtins.readFile (root + "/${path}"));
    in
    {
      name = (builtins.dirOf path) + "/${meta.filename}";
      value = pkgs.fetchurl {
        url = meta.download.url;
        outputHash = meta.download.hash;
        outputHashAlgo = meta.download.hash-format;
      };
    };

  importPackwizIndex =
    root: path:
    builtins.listToAttrs (
      builtins.map (
        file:
        if file.metafile or false == true then
          importPackwizMetaFile root file.file
        else
          {
            name = file.file;
            value = root + "/${file.file}";
          }
      ) (builtins.fromTOML (builtins.readFile (root + "/${path}"))).files
    );

  importPackwizModpack =
    root:
    let
      pack = builtins.fromTOML (builtins.readFile (root + "/pack.toml"));
    in
    pack // { files = importPackwizIndex root pack.index.file; };

  pack = importPackwizModpack ./packwiz;
  serverVersion = lib.replaceStrings [ "." ] [ "_" ] "fabric-${pack.versions.minecraft}";
in
{
  imports = [ ../. ];

  services.minecraft-servers.servers.Jakira = {
    enable = true;
    autoStart = false;
    jvmOpts = "-Xmx3G -Xms1G";
    package = pkgsNixMinecraft.fabricServers.${serverVersion}.override {
      jre_headless = pkgs.jdk17_headless;
      loaderVersion = pack.versions.fabric;
    };

    files = {
      "automodpack/automodpack-server.json".value = {
        modpackName = "Jakira";
        syncedFiles = [
          "/config/ambientsounds-client.json"
          "/config/artifacts/common.json5"
          "/config/bclib/client.json"
          "/config/carryon-common.json"
          "/config/emi.css"
          "/config/entity_model_features.json"
          "/config/fabric_loader_dependencies.json"
          "/config/inventoryprofilesnext/integrationHints/travelersbackpack.json"
          "/config/inventoryprofilesnext/inventoryprofiles.json"
          "/config/iris.properties"
          "/config/item_obliterator.json5"
          "/config/NoChatReports/NCR-Client.json"
          "/config/openloader/data"
          "/config/openloader/resources"
          "/config/presencefootsteps/userconfig.json"
          "/config/resourcepackoverrides.json"
          "/config/tconstruct-common.toml"
          "/config/universal-graves/config.json"
          "/config/xaerominimap.txt"
          "/config/xaeroworldmap.txt"
          "/mods/*.jar"
          "/options.txt"
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
        ui = {
          "showUpdateInfo [default: true]" = false;
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

      "config/tconstruct-common.toml".value = {
        gameplay = {
          shouldSpawnWithTinkersBook = false;
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
        {
          name = "cptjackl";
          uuid = "7f32461d-39aa-4e8a-b2a6-73a09cbf8fbf";
          level = 4;
        }
      ];
    };

    symlinks = pack.files // {
      "allowed_symlinks.txt" = pkgs.writeText "allowed_symlinks.txt" ''
        /nix/store
      '';

      "config/ambientsounds-client.json".value = {
        general.volume = 0.8;
        categories = {
          cave = 0.3;
          wind = 0.25;
        };
      };

      "config/emi.css" = pkgs.writeText "emi.css" ''
        #general {
          search-mod-name-by-default: true;
        }
      '';

      "config/entity_model_features.json".value = {
        # Fresh hand animations are incompatible with 3d skin layers
        preventFirstPersonHandAnimating = true;
      };

      # version must be first key - can't use builtins.toJSON
      "config/fabric_loader_dependencies.json" = pkgs.writeText "fabric_loader_dependencies.json" ''
        {
          "version": 1,
          "overrides": {
            "create": {
              "breaks": {
                "sound_physics_remastered": "<1.20.1-1.4.0"
              }
            }
          }
        }
      '';

      "config/inventoryprofilesnext/integrationHints/travelersbackpack.json".value = {
        "com.tiviacz.travelersbackpack.client.screen.TravelersBackpackHandledScreen" = {
          ignore = true;
        };
      };

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
        disableUpdateMessage = true;
        enableShaders = true;
        shaderPack = "ComplementaryUnbound.zip";
      };

      "config/NoChatReports/NCR-Client.json".value = {
        alwaysHideReportButton = true;
        showNCRButton = false;
        showReloadButton = false;
        showServerSafety = false;
        verifiedIconEnabled = false;
      };

      "config/presencefootsteps/userconfig.json".value = {
        volume = 20;
      };

      "config/resourcepackoverrides.json".value = {
        schema_version = 2;
        pack_overrides = {
          # Required
          "continuity:default" = {
            required = true;
            hidden = true;
          };

          "continuity:glass_pane_culling_fix" = {
            required = true;
            hidden = true;
          };

          "minecraft:supporteatinganimation" = {
            required = true;
            hidden = true;
          };

          "Moonlight Mods Dynamic Assets" = {
            required = true;
            hidden = true;
          };

          "presencefootsteps:default_sound_pack" = {
            required = true;
            hidden = true;
          };

          # Optional
          "extended_drawers:alt".force_compatible = true;

          # Unnecessary
          "$polymer-resources".hidden = true;

          # Legacy
          "create:legacy_copper".hidden = true;
          "extended_drawers:dev".hidden = true;
          "railways:legacy_palettes".hidden = true;
          "railways:legacy_semaphore".hidden = true;
          "vs_eureka:retro_helms".hidden = true;
        };
      };

      "config/xaerominimap.txt" = pkgs.writeText "xaerominimap.txt" ''
        ignoreUpdate:1
        updateNotification:false
        lockNorth:true
        minimapOpacity:75.0
        minimapShape:1
        module;id=xaerominimap:minimap;active=true;x=0;y=0;centered=false;fromRight=true;fromBottom=false;flippedVer=false;flippedHor=false;
      '';

      "config/xaeroworldmap.txt" = pkgs.writeText "xaeroworldmap.txt" ''
        ignoreUpdate:1
        updateNotification:false
      '';

      "options.txt" = pkgs.writeText "options.txt" ''
        version:3465
        joinedFirstServer:true
        skipMultiplayerWarning:true
        tutorialStep:none
        directionalAudio:true
      '';

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

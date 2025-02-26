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
    openFirewall = true;
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
          "/config/travelersbackpack.json5"
          "/config/universal-graves/config.json"
          "/config/xaerominimap.txt"
          "/config/xaeroworldmap.txt"
          "/mods/*.jar"
          "/options.txt"
          "/shaderpacks/*"
        ];

        allowEditsInFiles = [
          "/config/ambientsounds-client.json"
          "/config/emi.css"
          "/config/inventoryprofilesnext/inventoryprofiles.json"
          "/config/iris.properties"
          "/config/presencefootsteps/userconfig.json"
          "/config/xaerominimap.txt"
          "/config/xaeroworldmap.txt"
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

      "config/travelersbackpack.json5" = {
        format = pkgs.formats.json { };
        value = {
          backpackSettings = {
            toolSlotsAcceptableItems = [
              "ad_astra:ti_69"
              "ad_astra:wrench"
              "ad_astra:zip_gun"
              "ae2:certus_quartz_axe"
              "ae2:certus_quartz_cutting_knife"
              "ae2:certus_quartz_hoe"
              "ae2:certus_quartz_pickaxe"
              "ae2:certus_quartz_shovel"
              "ae2:certus_quartz_sword"
              "ae2:certus_quartz_wrench"
              "ae2:charged_staff"
              "ae2:color_applicator"
              "ae2:color_applicator"
              "ae2:entropy_manipulator"
              "ae2:fluix_axe"
              "ae2:fluix_hoe"
              "ae2:fluix_pickaxe"
              "ae2:fluix_shovel"
              "ae2:fluix_sword"
              "ae2:matter_cannon"
              "ae2:nether_quartz_axe"
              "ae2:nether_quartz_cutting_knife"
              "ae2:nether_quartz_hoe"
              "ae2:nether_quartz_pickaxe"
              "ae2:nether_quartz_shovel"
              "ae2:nether_quartz_sword"
              "ae2:nether_quartz_wrench"
              "ae2:network_tool"
              "betterarcheology:diamond_brush"
              "betterarcheology:iron_brush"
              "betterarcheology:netherite_brush"
              "betterend:aeternium_axe"
              "betterend:aeternium_hammer"
              "betterend:aeternium_hoe"
              "betterend:aeternium_pickaxe"
              "betterend:aeternium_shovel"
              "betterend:aeternium_sword"
              "betterend:diamond_hammer"
              "betterend:golden_hammer"
              "betterend:iron_hammer"
              "betterend:netherite_hammer"
              "betterend:terminite_axe"
              "betterend:terminite_hammer"
              "betterend:terminite_hoe"
              "betterend:terminite_pickaxe"
              "betterend:terminite_shovel"
              "betterend:terminite_sword"
              "betterend:thallasium_axe"
              "betterend:thallasium_hammer"
              "betterend:thallasium_hoe"
              "betterend:thallasium_pickaxe"
              "betterend:thallasium_shovel"
              "betterend:thallasium_sword"
              "betternether:cincinnasite_axe"
              "betternether:cincinnasite_axe_diamond"
              "betternether:cincinnasite_hoe"
              "betternether:cincinnasite_hoe_diamond"
              "betternether:cincinnasite_pickaxe"
              "betternether:cincinnasite_pickaxe_diamond"
              "betternether:cincinnasite_shears"
              "betternether:cincinnasite_shovel"
              "betternether:cincinnasite_shovel_diamond"
              "betternether:cincinnasite_sword"
              "betternether:cincinnasite_sword_diamond"
              "betternether:flaming_ruby_axe"
              "betternether:flaming_ruby_hoe"
              "betternether:flaming_ruby_pickaxe"
              "betternether:flaming_ruby_shovel"
              "betternether:flaming_ruby_sword"
              "betternether:nether_ruby_axe"
              "betternether:nether_ruby_boots"
              "betternether:nether_ruby_hoe"
              "betternether:nether_ruby_pickaxe"
              "betternether:nether_ruby_shovel"
              "betternether:nether_ruby_sword"
              "create:extendo_grip"
              "create:handheld_worldshaper"
              "create:linked_controller"
              "create:potato_cannon"
              "create:wand_of_symmetry"
              "create:wrench"
              "create_dd:deforester_saw"
              "create_dd:forest_ravager"
              "createbigcannons:block_armor_inspection_tool"
              "createbigcannons:cannon_crafting_wand"
              "createbigcannons:ram_rod"
              "createbigcannons:worm"
              "deeperdarker:resonarium_axe"
              "deeperdarker:resonarium_hoe"
              "deeperdarker:resonarium_pickaxe"
              "deeperdarker:resonarium_shovel"
              "deeperdarker:resonarium_sword"
              "deeperdarker:sculk_transmitter"
              "deeperdarker:sonorous_staff"
              "deeperdarker:warden_axe"
              "deeperdarker:warden_hoe"
              "deeperdarker:warden_pickaxe"
              "deeperdarker:warden_shovel"
              "deeperdarker:warden_sword"
              "endermanoverhaul:corrupted_blade"
              "exposure:camera"
              "extended_drawers:dupe_wand"
              "farmersdelight:diamond_knife"
              "farmersdelight:flint_knife"
              "farmersdelight:golden_knife"
              "farmersdelight:iron_knife"
              "farmersdelight:netherite_knife"
              "handcrafted:hammer"
              "naturalist:bug_net"
              "porting_lib:area_selector"
              "supplementaries:altimeter"
              "supplementaries:bubble_blower"
              "supplementaries:flute"
              "supplementaries:slingshot"
              "supplementaries:wrench"
              "tconstruct:broad_axe"
              "tconstruct:cleaver"
              "tconstruct:crossbow"
              "tconstruct:dagger"
              "tconstruct:earth_slime_sling"
              "tconstruct:earth_staff"
              "tconstruct:ender_slime_sling"
              "tconstruct:excavator"
              "tconstruct:hand_axe"
              "tconstruct:ichor_slime_sling"
              "tconstruct:ichor_staff"
              "tconstruct:kama"
              "tconstruct:longbow"
              "tconstruct:mattock"
              "tconstruct:pickadze"
              "tconstruct:pickaxe"
              "tconstruct:scythe"
              "tconstruct:sky_slime_sling"
              "tconstruct:sky_staff"
              "tconstruct:sledge_hammer"
              "tconstruct:sword"
              "tconstruct:vein_hammer"
            ];

            # Server fails to start without this set
            trinketsIntegration = true;
          };
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

        #ui {
          left-sidebar-subpanels: craftables 10;
        }
      '';

      "config/inventoryprofilesnext/integrationHints/travelersbackpack.json".value = {
        "com.tiviacz.travelersbackpack.client.screens.BackpackScreen" = {
          ignore = true;
        };
        "com.tiviacz.travelersbackpack.inventory.menu.BackpackItemMenu" = {
          ignore = true;
        };
        "com.tiviacz.travelersbackpack.client.screens.BackpackSettingsScreen" = {
          ignore = true;
        };
        "com.tiviacz.travelersbackpack.inventory.menu.BackpackSettingsMenu" = {
          ignore = true;
        };
      };

      "config/inventoryprofilesnext/inventoryprofiles.json".value = {
        ModSettings = {
          enable_updates_check = false;
          first_run = false;
          sort_order = "RAW_ID";
        };

        Hotkeys = {
          sort_inventory.main.keys = "R,S";
        };

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
        autoConvertWaypointDistanceToKmThreshold:10000
        compassLocation:2
        displayCurrentClaim:true
        ignoreUpdate:1
        lightOverlayColor:0
        lockNorth:true
        minimapOpacity:75.0
        minimapShape:0
        northCompassColor:-1
        updateNotification:false
        waypointsBottom:true
        waypointsDistanceExp:8
        waypointsDistanceMin:25.0
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
      level-seed = 1989853394042396715;
    };
  };
}

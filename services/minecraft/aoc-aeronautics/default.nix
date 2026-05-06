{ pkgs, pkgsNixMinecraft, ... }:

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

  modpack = pkgs.fetchzip {
    url = "https://mediafilez.forgecdn.net/files/8026/776/Aoc_Aeronautics_v1.4_serverpack.zip";
    stripRoot = false;
    hash = "sha256-P0O13qAYJkf85AxUpLjkeeSsmRYay4XrETrfrJ+EHtc=";
  };

  serverVersion = "neoforge-1_21_1-21_1_228";
in
{
  imports = [ ../. ];

  services.minecraft-servers.servers.aoc-aeronautics = {
    enable = true;
    autoStart = false;
    package = pkgsNixMinecraft.neoforgeServers.${serverVersion};

    symlinks = pack.files // {
      "config/carryon-common.json".value = {
        blacklist = {
          forbiddenTiles = [
            "#c:relocation_not_supported"
            "#neoforge:immovable"
            "#neoforge:relocation_not_supported"
            "copycats:*"
            "create*:*"
            "create:*"
            "create_enchantment_industry:*"
            "farmersdelight:*"
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
          ];
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
          audio_replace_failed_notification.value = false;
          auto_refill_enable_per_slot_config.value = false;
          number_of_notifications = 1;
          visual_replace_failed_notification.value = false;
        };
      };

      "datapacks" = "${modpack}/datapacks";
      "defaultconfigs" = "${modpack}/defaultconfigs";
      "server-icon.png" = "${modpack}/server-icon.png";
    };

    files = {
      "automodpack/automodpack-server.json".value = {
        DO_NOT_CHANGE_IT = 2;
        modpackName = "aoc-aeronautics";
        syncedFiles = [
          "/mods/*.jar"
        ];

        allowEditsInFiles = [
          "/config/emi.css"
          "/config/inventoryprofilesnext/inventoryprofiles.json"
          "/config/quark-common.toml"
        ];
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

    serverProperties = {
      allow-flight = true;
      difficulty = "normal";
      enforce-secure-profile = false;
      max-players = 5;
      max-tick-time = 120000;
      server-port = 25564;
      snooper-enabled = false;
    };

    jvmOpts = "-Xmx8G -Xms4G";
  };

  systemd.services.minecraft-server-aoc-aeronautics = {
    preStart = ''
      rm -rf config
      cp -r --no-preserve=mode ${modpack}/config config

      sed \
        -e 's/management = true/management = false/' \
        -e 's/experimental = true/experimental = false/' \
        -i config/quark-common.toml

      sed \
        -e 's/red_merchant_spawn_multiplier = 1.0/red_merchant_spawn_multiplier = 0.0/' \
        -i config/supplementaries-common.toml

      rm -rf mods
      mkdir mods
      ln -s ${modpack}/mods/* mods
    '';
  };

  systemd.sockets.minecraft-server-aoc-aeronautics-proxy = {
    wantedBy = [ "sockets.target" ];
    requires = [ "network.target" ];
    listenStreams = [ "25565" ];
  };

  systemd.services.minecraft-server-aoc-aeronautics-proxy = {
    requires = [ "minecraft-server-aoc-aeronautics.service" ];
    after = [ "minecraft-server-aoc-aeronautics.service" ];
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

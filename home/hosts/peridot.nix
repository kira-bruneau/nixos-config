{
  config,
  lib,
  pkgs,
  ...
}:

let
  json2vdf = pkgs.writers.writePython3 "json2vdf.py" { libraries = [ pkgs.python3Packages.vdf ]; } ''
    import json
    import sys
    import vdf


    def json2vdf(data):
        if isinstance(data, dict):
            return {k: json2vdf(v) for k, v in data.items()}
        if isinstance(data, list):
            return {str(k): json2vdf(v) for k, v in enumerate(data)}
        else:
            return data


    with open(sys.argv[1]) as fp:
        data = json.load(fp)

    data = json2vdf(data)

    with open(sys.argv[2], "wb") as fp:
        vdf.binary_dump(data, fp)
  '';

  vdf =
    { }:
    {
      type =
        with lib.types;
        let
          valueType =
            nullOr (oneOf [
              int
              float
              str
              path
              # TODO:
              # uint64
              # int64
              # widestring
              # color
              # pointer
              (attrsOf valueType)
              (listOf valueType)
            ])
            // {
              description = "VDF value";
            };
        in
        valueType;

      generate =
        name: value:
        pkgs.runCommand name
          {
            value = builtins.toJSON value;
            passAsFile = [ "value" ];
          }
          ''
            ${json2vdf} "$valuePath" "$out"
          '';
    };

  settingsFormat = vdf { };

  xdgProfile =
    name:
    "XDG_CONFIG_HOME=${config.home.homeDirectory}/.${name}/config XDG_DATA_HOME=${config.home.homeDirectory}/.${name}/data XDG_STATE_HOME=${config.home.homeDirectory}/.${name}/state";

  # TODO: Compute grid art compatible id
  # https://gaming.stackexchange.com/questions/386882/how-do-i-find-the-appid-for-a-non-steam-game-on-steam
  # generateAppId =
  #   name:
  #   -1
  #   - (builtins.bitAnd 2147483647
  #     (builtins.fromTOML "v=0x${builtins.substring 0 8 (builtins.hashString "sha256" name)}").v
  #   );

  generateAppId =
    name:
    {
      "BigChadGuys Plus" = -103678894;
      "Cemu" = -1562815367;
      "Clone Hero" = -314732879;
      "Discord" = -740890966;
      "Dolphin Emulator" = -1761564564;
      "Dropout" = -1539404924;
      "Element" = -1815128494;
      "Jellyfin Media Player" = -567380782;
      "Librewolf" = -1974293946;
      "Mario Kart 8 Deluxe" = -1565192715;
      "Mario Kart 8" = -1565192714;
      "Mario Kart: Double Dash" = -1565192713;
      "Mario Party 4" = -1565192712;
      "Moonlight" = -114896551;
      "Pikmin 3 Deluxe" = -1565192716;
      "Prism Launcher" = -812576484;
      "qBittorrent" = -909108272;
      "Rosalie's Mupen GUI" = -1234567891;
      "sudachi" = -592608668;
      "Super Smash Bros. Ultimate" = -1737320186;
      "The Legend of Zelda: Echoes of Wisdom" = -1565192717;
      "The Legend Of Zelda: Majora's Mask" = -1234567890;
      "The Legend Of Zelda: The Wind Waker" = -1826256805;
      "Youtube" = -2035226226;
    }
    .${name};

  generateShortcut =
    name: config:
    {
      appid = generateAppId name;
      appname = name;
      AllowOverlay = 0;
    }
    // config
    // {
      Exe = "env";
      LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= ${config.LaunchOptions}";
    };

  generateShortcuts = shortcuts: lib.mapAttrsToList generateShortcut shortcuts;

  common = {
    "BigChadGuys Plus".LaunchOptions = "prismlauncher --launch \"BigChadGuys Plus\"";
    "Cemu".LaunchOptions = "Cemu";
    "Clone Hero".LaunchOptions = "clonehero";
    "Discord".LaunchOptions = "Discord";
    "Dolphin Emulator".LaunchOptions = "QT_QPA_PLATFORM=xcb dolphin-emu";
    "Dropout".LaunchOptions = "GDK_SCALE=2 librewolf --new-window --kiosk https://www.dropout.tv -P Jackwolf";
    "Librewolf".LaunchOptions = "librewolf";
    "Jellyfin Media Player".LaunchOptions = "jellyfinmediaplayer --tv --scale-factor 2";
    "Mario Kart 8 Deluxe".LaunchOptions = "sudachi -f -g '${config.home.homeDirectory}/Games/Sudachi/Mario Kart 8 Deluxe[0100152000022000][US][v0].nsp'";
    "Mario Kart 8".LaunchOptions = "Cemu --fullscreen --title-id 000500001010ec00";
    "Mario Kart: Double Dash".LaunchOptions = "dolphin-emu -b -e '${config.home.homeDirectory}/Games/Dolphin/Mario Kart - Double Dash.gcm'";
    "Mario Party 4".LaunchOptions = "dolphin-emu -b -e '${config.home.homeDirectory}/Games/Dolphin/MarioParty4.iso'";
    "Moonlight".LaunchOptions = "moonlight";
    "Pikmin 3 Deluxe".LaunchOptions = "sudachi -f -g '${config.home.homeDirectory}/Games/Sudachi/Pikmin 3 Deluxe [0100F4C009322000][US][v0].nsp'";
    "Prism Launcher".LaunchOptions = "prismlauncher";
    "qBittorrent".LaunchOptions = "qbittorrent";
    "Rosalie's Mupen GUI".LaunchOptions = "RMG --fullscreen";
    "sudachi".LaunchOptions = "sudachi -f";
    "Super Smash Bros. Ultimate".LaunchOptions = "sudachi -f -g ${config.home.homeDirectory}/Games/Sudachi/50e90d167d20f348cd4793aad9401283.nca";
    "The Legend of Zelda: Echoes of Wisdom".LaunchOptions = "sudachi -f -g '${config.home.homeDirectory}/Games/Sudachi/The Legend of Zelda Echoes of Wisdom [01008CF01BAAC000][v0].nsp'";
    "The Legend Of Zelda: Majora's Mask".LaunchOptions = "2s2h";
    "The Legend Of Zelda: The Wind Waker".LaunchOptions = "Cemu --fullscreen --title-id 0005000010143500";
    "Youtube".LaunchOptions = "GDK_SCALE=2 librewolf --new-window --kiosk https://www.youtube.com/tv";
  };
in
{
  imports = [
    ../environments/bluetooth.nix
    ../environments/gaming.nix
    ../environments/gui/gnome.nix
  ];

  home.stateVersion = "24.05";

  # Enable CEF remote debugging for decky-loader
  xdg.dataFile."Steam/.cef-enable-remote-debugging".text = "";

  xdg.dataFile."Steam/userdata/84026532/config/shortcuts.vdf" = {
    force = true;
    source = settingsFormat.generate "shortcuts.vdf" {
      shortcuts = generateShortcuts (
        common
        // {
          "BigChadGuys Plus".LaunchOptions = "${common."BigChadGuys Plus".LaunchOptions} --profile daxvena";
          "Discord".LaunchOptions = "${xdgProfile "kira"} ${common."Discord".LaunchOptions}";
          "Librewolf".LaunchOptions = "${common."Librewolf".LaunchOptions} -P Kirawolf";
        }
      );
    };
  };

  xdg.dataFile."Steam/userdata/80252694/config/shortcuts.vdf" = {
    force = true;
    source = settingsFormat.generate "shortcuts.vdf" {
      shortcuts = generateShortcuts (
        common
        // {
          "BigChadGuys Plus".LaunchOptions = "${common."BigChadGuys Plus".LaunchOptions} --profile CptJackL";
          "Discord".LaunchOptions = "${xdgProfile "jack"} ${common."Discord".LaunchOptions}";
          "Librewolf".LaunchOptions = "${common."Librewolf".LaunchOptions} -P Jackwolf";
        }
      );
    };
  };

  dconf.settings = {
    # Enable on-screen keyboard
    "org/gnome/desktop/a11y/applications" = {
      screen-keyboard-enabled = true;
    };

    "org/gnome/shell" = {
      favorite-apps = [
        "org.gnome.Nautilus.desktop"
        "librewolf.desktop"
        "jackwolf.desktop"
        "steam.desktop"
        "com.github.iwalton3.jellyfin-media-player.desktop"
        "org.gnome.Console.desktop"
        "dev.vlinkz.NixSoftwareCenter.desktop"
      ];
    };
  };

  systemd.user = {
    services.update-jellyfin-hero = {
      Unit = {
        Description = "Automatically update jellyfin's hero image";
      };

      Service = {
        Type = "oneshot";
        ExecStart = "${lib.getExe pkgs.curl} http://quartz:8096/Branding/Splashscreen -o ${config.home.homeDirectory}/.local/share/Steam/userdata/84026532/config/grid/3727586514_hero.png";
      };
    };

    timers.update-jellyfin-hero = {
      Unit = {
        Description = "Automatically update jellyfin's hero image";
      };

      Timer = {
        OnCalendar = "daily";
        Persistent = "true";
      };

      Install = {
        WantedBy = [ "timers.target" ];
      };
    };
  };

  programs.librewolf.policies.ExtensionSettings."{d2bcedce-889b-4d53-8ce9-493d8f78612a}" = {
    installation_mode = "force_installed";
    install_url = "https://addons.mozilla.org/firefox/downloads/latest/youtube-for-tv/latest.xpi";
  };
}

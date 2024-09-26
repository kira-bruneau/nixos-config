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
      shortcuts = [
        {
          appid = -103678894;
          appname = "BigChadGuys Plus";
          Exe = "env";
          icon = "${config.home.homeDirectory}/.local/share/Steam/userdata/84026532/config/grid/4191288402_icon.png";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= prismlauncher --profile daxvena --launch \"BigChadGuys Plus\"";
          AllowOverlay = 0;
        }
        {
          appid = -567380782;
          appname = "Jellyfin Media Player";
          Exe = "env";
          icon = "${config.home.homeDirectory}/.local/share/Steam/userdata/84026532/config/grid/3727586514_icon.png";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= jellyfinmediaplayer --tv --scale-factor 2";
          AllowOverlay = 0;
        }
        {
          appid = -114896551;
          appname = "Moonlight";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= moonlight";
          AllowOverlay = 0;
        }
        {
          appid = -812576484;
          appname = "Prism Launcher";
          Exe = "env";
          icon = "${config.home.homeDirectory}/.local/share/Steam/userdata/84026532/config/grid/3482390812_icon.png";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= prismlauncher";
          AllowOverlay = 0;
        }
        {
          appid = -314732879;
          appname = "Clone Hero";
          Exe = "env";
          icon = "${config.home.homeDirectory}/.local/share/Steam/userdata/84026532/config/grid/3980234417_icon.png";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= clonehero";
          AllowOverlay = 0;
        }
        {
          appid = -740890966;
          appname = "Discord";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= XDG_CONFIG_HOME=${config.home.homeDirectory}/.kira/config XDG_DATA_HOME=${config.home.homeDirectory}/.kira/data XDG_STATE_HOME=${config.home.homeDirectory}/.kira/state Discord";
          AllowOverlay = 0;
        }
        {
          appid = -1815128494;
          appname = "Element";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= XDG_CONFIG_HOME=${config.home.homeDirectory}/.kira/config XDG_DATA_HOME=${config.home.homeDirectory}/.kira/data XDG_STATE_HOME=${config.home.homeDirectory}/.kira/state element-desktop";
          AllowOverlay = 0;
        }
        {
          appid = -1562815367;
          appname = "Cemu";
          Exe = "env";
          icon = "${config.home.homeDirectory}/.local/share/Steam/userdata/84026532/config/grid/2732151929_icon.png";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= Cemu";
          AllowOverlay = 0;
        }
        {
          appid = -1761564564;
          appname = "Dolphin Emulator";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= QT_QPA_PLATFORM=xcb dolphin-emu";
          AllowOverlay = 0;
        }
        {
          appid = -1974293946;
          appname = "Firefox";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= firefox";
          AllowOverlay = 0;
        }
        {
          appid = -909108272;
          appname = "qBittorrent";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= qbittorrent";
          AllowOverlay = 0;
        }
        {
          appid = -2035226226;
          appname = "Youtube";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= GDK_SCALE=2 firefox --new-window --kiosk https://www.youtube.com";
          AllowOverlay = 0;
        }
        {
          appid = -592608668;
          appname = "sudachi";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= sudachi -f";
          AllowOverlay = 0;
        }
        {
          appid = -1539404924;
          appname = "Dropout";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= GDK_SCALE=2 firefox -P Jackfox --new-window --kiosk https://www.dropout.tv";
          AllowOverlay = 0;
        }
        {
          appid = -1737320186;
          appname = "Super Smash Bros. Ultimate";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= sudachi -f -g ${config.home.homeDirectory}/Games/Sudachi/50e90d167d20f348cd4793aad9401283.nca";
          AllowOverlay = 0;
        }
        {
          appid = -1565192715;
          appname = "Mario Kart 8 Deluxe";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= sudachi -f -g '${config.home.homeDirectory}/Games/Sudachi/Mario Kart 8 Deluxe[0100152000022000][US][v0].nsp'";
          AllowOverlay = 0;
        }
        {
          appid = -1565192716;
          appname = "Pikmin 3 Deluxe";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= sudachi -f -g '${config.home.homeDirectory}/Games/Sudachi/Pikmin 3 Deluxe [0100F4C009322000][US][v0].nsp'";
          AllowOverlay = 0;
        }
        {
          appid = -1565192717;
          appname = "The Legend of Zelda: Echoes of Wisdom";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= sudachi -f -g '${config.home.homeDirectory}/Games/Sudachi/The Legend of Zelda Echoes of Wisdom [01008CF01BAAC000][v0].nsp'";
          AllowOverlay = 0;
        }
        {
          appid = -1826256805;
          appname = "The Legend Of Zelda The Wind Waker";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= Cemu --fullscreen --title-id 0005000010143500";
          AllowOverlay = 0;
        }
        {
          appid = -1565192714;
          appname = "Mario Kart 8";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= Cemu --fullscreen --title-id 000500001010ec00";
          AllowOverlay = 0;
        }
        {
          appid = -1234567890;
          appname = "Majora's Mask";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= 2s2h";
          AllowOverlay = 0;
        }
        {
          appid = -1234567891;
          appname = "Rosalie's Mupen GUI";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= RMG --fullscreen";
          AllowOverlay = 0;
        }
      ];
    };
  };

  xdg.dataFile."Steam/userdata/80252694/config/shortcuts.vdf" = {
    force = true;
    source = settingsFormat.generate "shortcuts.vdf" {
      shortcuts = [
        {
          appid = -103678894;
          appname = "BigChadGuys Plus";
          Exe = "env";
          icon = "${config.home.homeDirectory}/.local/share/Steam/userdata/84026532/config/grid/4191288402_icon.png";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= prismlauncher --profile CptJackL --launch \"BigChadGuys Plus\"";
          AllowOverlay = 0;
        }
        {
          appid = -567380782;
          appname = "Jellyfin Media Player";
          Exe = "env";
          icon = "${config.home.homeDirectory}/.local/share/Steam/userdata/84026532/config/grid/3727586514_icon.png";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= jellyfinmediaplayer --tv --scale-factor 2";
          AllowOverlay = 0;
        }
        {
          appid = -114896551;
          appname = "Moonlight";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= moonlight";
          AllowOverlay = 0;
        }
        {
          appid = -812576484;
          appname = "Prism Launcher";
          Exe = "env";
          icon = "${config.home.homeDirectory}/.local/share/Steam/userdata/84026532/config/grid/3482390812_icon.png";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= prismlauncher";
          AllowOverlay = 0;
        }
        {
          appid = -314732879;
          appname = "Clone Hero";
          Exe = "env";
          icon = "${config.home.homeDirectory}/.local/share/Steam/userdata/84026532/config/grid/3980234417_icon.png";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= clonehero";
          AllowOverlay = 0;
        }
        {
          appid = -740890966;
          appname = "Discord";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= XDG_CONFIG_HOME=${config.home.homeDirectory}/.jack/config XDG_DATA_HOME=${config.home.homeDirectory}/.jack/data XDG_STATE_HOME=${config.home.homeDirectory}/.jack/state Discord";
          AllowOverlay = 0;
        }
        {
          appid = -1815128494;
          appname = "Element";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= XDG_CONFIG_HOME=${config.home.homeDirectory}/.jack/config XDG_DATA_HOME=${config.home.homeDirectory}/.jack/data XDG_STATE_HOME=${config.home.homeDirectory}/.jack/state element-desktop";
          AllowOverlay = 0;
        }
        {
          appid = -1562815367;
          appname = "Cemu";
          Exe = "env";
          icon = "${config.home.homeDirectory}/.local/share/Steam/userdata/84026532/config/grid/2732151929_icon.png";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= Cemu";
          AllowOverlay = 0;
        }
        {
          appid = -1761564564;
          appname = "Dolphin Emulator";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= QT_QPA_PLATFORM=xcb dolphin-emu";
          AllowOverlay = 0;
        }
        {
          appid = -1974293946;
          appname = "Firefox";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= firefox -P Jackfox";
          AllowOverlay = 0;
        }
        {
          appid = -909108272;
          appname = "qBittorrent";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= qbittorrent";
          AllowOverlay = 0;
        }
        {
          appid = -2035226226;
          appname = "Youtube";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= GDK_SCALE=2 firefox -P Jackfox --new-window --kiosk https://www.youtube.com";
          AllowOverlay = 0;
        }
        {
          appid = -592608668;
          appname = "sudachi";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= sudachi -f";
          AllowOverlay = 0;
        }
        {
          appid = -1539404924;
          appname = "Dropout";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= GDK_SCALE=2 firefox -P Jackfox --new-window --kiosk https://www.dropout.tv";
          AllowOverlay = 0;
        }
        {
          appid = -1737320186;
          appname = "Super Smash Bros. Ultimate";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= sudachi -f -g ${config.home.homeDirectory}/Games/Sudachi/50e90d167d20f348cd4793aad9401283.nca";
          AllowOverlay = 0;
        }
        {
          appid = -1565192715;
          appname = "Mario Kart 8 Deluxe";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= sudachi -f -g '${config.home.homeDirectory}/Games/Sudachi/Mario Kart 8 Deluxe[0100152000022000][US][v0].nsp'";
          AllowOverlay = 0;
        }
        {
          appid = -1565192716;
          appname = "Pikmin 3 Deluxe";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= sudachi -f -g '${config.home.homeDirectory}/Games/Sudachi/Pikmin 3 Deluxe [0100F4C009322000][US][v0].nsp'";
          AllowOverlay = 0;
        }
        {
          appid = -1565192717;
          appname = "The Legend of Zelda: Echoes of Wisdom";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= sudachi -f -g '${config.home.homeDirectory}/Games/Sudachi/The Legend of Zelda Echoes of Wisdom [01008CF01BAAC000][v0].nsp'";
          AllowOverlay = 0;
        }
        {
          appid = -1826256805;
          appname = "The Legend Of Zelda The Wind Waker";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= Cemu --fullscreen --title-id 0005000010143500";
          AllowOverlay = 0;
        }
        {
          appid = -1565192714;
          appname = "Mario Kart 8";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= Cemu --fullscreen --title-id 000500001010ec00";
          AllowOverlay = 0;
        }
        {
          appid = -1234567890;
          appname = "Majora's Mask";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= 2s2h";
          AllowOverlay = 0;
        }
        {
          appid = -1234567891;
          appname = "Rosalie's Mupen GUI";
          Exe = "env";
          LaunchOptions = "LD_PRELOAD= LD_LIBRARY_PATH= RMG --fullscreen";
          AllowOverlay = 0;
        }
      ];
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
        "firefox.desktop"
        "jackfox.desktop"
        "steam.desktop"
        "com.github.iwalton3.jellyfin-media-player.desktop"
        "org.gnome.Console.desktop"
        "dev.vlinkz.NixSoftwareCenter.desktop"
      ];
    };
  };
}

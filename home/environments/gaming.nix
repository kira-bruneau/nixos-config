{ pkgs, pkgsKiraNur, pkgsYuzu, ... }:

let
  mario64Rom = pkgs.fetchurl {
    url = "https://ipfs.io/ipfs/QmWGw3Qu72FYoPpBTKMDJqsmhBnieBsXGkimGRd5bRBUDe";
    hash = "sha256-F84Hc0PGEz+Mny1tbZpKtiyM0qpXxArqH0kLTIuyHZE=";
  };
in
{
  imports = [
    ../programs/lutris
    ../programs/mupen64plus
  ];

  home.packages = with pkgs; [
    # Games & Launchers
    clonehero
    pkgsKiraNur.pokemmo-installer
    prismlauncher
    pkgsKiraNur.protontricks
    (sm64ex.overrideAttrs (attrs: {
      makeFlags = attrs.makeFlags ++ [
        "BETTERCAMERA=1"
      ];

      preBuild = ''
        patchShebangs extract_assets.py
        ln -s ${mario64Rom} ./baserom.us.z64
      '';
    }))
    ukmm
    vvvvvv

    # Emulators
    dolphinEmuMaster
    pkgsYuzu.yuzu
    winetricks
    wineWowPackages.staging
    winetricks

    # Recording
    obs-studio

    # Overlay / Post-processing
    goverlay
    mangohud
    vkBasalt
  ];

  wayland.windowManager.sway.config.assigns."5" = [
    { app_id = "^org.dolphin-emu.$"; }
    { app_id = "^org.prismlauncher.PrismLauncher$"; }
    { class = "^cemu.exe$"; }
    { class = "^dolphin-emu$"; }
    { class = "^Steam$"; }
  ];

  home.file = {
    "Games/ROMs/Super Mario 64.z64".source = mario64Rom;
  };
}

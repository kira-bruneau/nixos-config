{ config, pkgs, ... }:

{
  home.packages = with pkgs; with nur.repos.metadark; let
    wineWowStaging = wineWowPackages.staging;
  in [
    # Games & Launchers
    bcml
    clonehero # unfree
    (lutris.override { # unfreeRedistributable with steamSupport = true
      lutris-unwrapped = lutris-unwrapped.override {
        wine = wineWowStaging;
      };
    })
    multimc
    pokemmo-installer
    protontricks # unfreeRedistributable (from steam-run)
    runelite
    runescape-launcher # unfree
    (sm64ex.override {
      compileFlags = [
        "BETTERCAMERA=1"
      ];
    })
    steam # unfreeRedistributable
    steam-run # unfreeRedistributable
    VVVVVV # unfree

    # Emulators
    dolphinEmuMaster
    mupen64plus
    wineWowStaging
    (winetricks.override { wine = wineWowStaging; })

    # Controllers
    xwiimote

    # Chat
    discord # unfree

    # Recording
    obs-studio

    # Overlay / Post-processing
    goverlay
    mangohud
    vkBasalt
  ];
}

{ config, pkgs, ... }:

{
  home.packages = with pkgs; with nur.repos.metadark; let
    wineWowStagingFull = wineWowPackages.full.override {
      wineRelease = "staging";
    };
  in [
    # Games & Launchers
    bcml
    clonehero # unfree
    (lutris.override { # unfreeRedistributable with steamSupport = true
      lutris-unwrapped = lutris-unwrapped.override {
        wine = wineWowStagingFull;
      };
    })
    multimc
    pokemmo-installer
    protontricks
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
    wineWowStagingFull
    (winetricks.override { wine = wineWowStagingFull; })

    # Controllers
    xwiimote

    # Chat
    discord # unfree

    # Recording
    obs-studio

    # Overlay / Post-processing
    goverlay
    vkBasalt
  ];
}

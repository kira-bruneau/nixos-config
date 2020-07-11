{ config, pkgs, ... }:

{
  home.packages = with pkgs; with nur.repos.metadark; [
    # Games & Launchers
    clonehero
    lutris
    multimc
    runelite
    runescape-launcher
    steam steam-run protontricks
    VVVVVV

    # Emulators
    dolphinEmuMaster
    mupen64plus
    (winetricks.override { wine = wineWowPackages.staging; })
    wineWowPackages.staging

    # Controllers
    xwiimote

    # Chat
    discord

    # Recording
    obs-studio

    # Overlay / Post-processing
    vkBasalt
  ];
}

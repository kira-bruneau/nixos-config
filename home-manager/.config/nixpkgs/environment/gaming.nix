{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    # Games & Launchers
    lutris
    multimc
    nur.repos.metadark.clonehero
    nur.repos.metadark.VVVVVV
    runelite
    steam steam-run protontricks

    # Emulators
    dolphinEmuMaster
    mupen64plus
    wineWowPackages.staging
    (winetricks.override { wine = wineWowPackages.staging; })

    # Controllers
    xwiimote

    # Chat
    discord
  ];
}

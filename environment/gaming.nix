{ pkgs, ... }:

{
  imports = [
    ../package/lutris
    ../package/mupen64plus
  ];

  home.packages = with pkgs; [
    # Games & Launchers
    bcml
    clonehero # unfree
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
    wineWowPackages.staging
    (winetricks.override { wine = wineWowPackages.staging; })

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

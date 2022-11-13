{ pkgs, ... }:

{
  imports = [
    ../package/bcml
    ../package/lutris
    ../package/mupen64plus
  ];

  home.packages = with pkgs; [
    # Games & Launchers
    clonehero
    pokemmo-installer
    prismlauncher
    protontricks
    (sm64ex.overrideAttrs (attrs: {
      makeFlags = attrs.makeFlags ++ [
        "BETTERCAMERA=1"
      ];
    }))
    steam
    steam-run
    VVVVVV

    # Emulators
    dolphinEmuMaster
    wineWowPackages.staging
    winetricks

    # Controllers
    xwiimote

    # Chat
    discord

    # Recording
    obs-studio

    # Overlay / Post-processing
    goverlay
    mangohud
    vkBasalt
  ];
}

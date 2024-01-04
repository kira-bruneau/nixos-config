{ pkgs, ... }:

{
  imports = [
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
    ukmm
    vvvvvv

    # Emulators
    dolphinEmuMaster
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
}

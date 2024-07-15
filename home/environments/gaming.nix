{
  pkgs,
  pkgsUnstable,
  pkgsKiraNur,
  ...
}:

let
  mario64Rom = pkgs.fetchurl {
    url = "https://ipfs.io/ipfs/QmWGw3Qu72FYoPpBTKMDJqsmhBnieBsXGkimGRd5bRBUDe";
    hash = "sha256-F84Hc0PGEz+Mny1tbZpKtiyM0qpXxArqH0kLTIuyHZE=";
  };
in
{
  imports = [
    ../programs/lutris
    ../programs/prismlauncher
    ../programs/rmg
  ];

  home.packages = with pkgs; [
    # Games & Launchers
    pkgsUnstable._2ship2harkinian
    clonehero
    pkgsKiraNur.pokemmo-installer
    pkgsKiraNur.protontricks
    (sm64ex.overrideAttrs (attrs: {
      makeFlags = attrs.makeFlags ++ [ "BETTERCAMERA=1" ];
      preBuild = ''
        patchShebangs extract_assets.py
        ln -s ${mario64Rom} ./baserom.us.z64
      '';
    }))
    ukmm
    vvvvvv

    # Emulators
    cemu
    dolphinEmuMaster
    pkgsKiraNur.sudachi
    winetricks
    wineWowPackages.staging

    # Modding
    packwiz

    # Recording
    obs-studio

    # Overlay / Post-processing
    goverlay
    mangohud
    vkBasalt
  ];

  wayland.windowManager.sway.config.assigns."5" = [
    { app_id = "^org.dolphin-emu.$"; }
    { class = "^cemu.exe$"; }
    { class = "^dolphin-emu$"; }
    { class = "^steam$"; }
  ];

  home.file = {
    "Games/ROMs/Super Mario 64.z64".source = mario64Rom;
  };
}

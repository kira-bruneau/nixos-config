{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Games & Launchers
    lutris
    multimc
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

  nixpkgs.config.allowUnfree = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;
}

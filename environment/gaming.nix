{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Games & Launchers
    multimc
    steam
    steam-run
    protontricks

    # Emulators
    dolphinEmuMaster
    mupen64plus

    # Controllers
    xwiimote

    # Chat
    discord
  ];

  nixpkgs.config.allowUnfree = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;
}

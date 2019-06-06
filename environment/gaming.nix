{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    discord
    dolphinEmuMaster
    multimc
    mupen64plus
    protontricks
    steam
    steam-run
  ];

  nixpkgs.config.allowUnfree = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;
}

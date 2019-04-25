{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    discord
    dolphinEmu
    multimc
    mupen64plus
    steam
  ];

  nixpkgs.config.allowUnfree = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;
}

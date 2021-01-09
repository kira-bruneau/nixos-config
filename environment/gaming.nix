{ config, pkgs, ... }:

{
  # Support for running 32bit games
  # See https://nixos.wiki/wiki/Steam
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
}

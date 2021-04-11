{ config, pkgs, ... }:

{
  # Support for running 32bit games
  # See https://nixos.wiki/wiki/Steam
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];

  # Better driver for Xbox One controllers
  hardware.xpadneo.enable = true;

  # Set higher file limit for wine esync support
  security.pam.loginLimits = [
    {
      domain = "*";
      item = "nofile";
      type = "-";
      value = "524288";
    }
  ];
}

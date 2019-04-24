{ config, pkgs, ... }:

{
  fonts.fonts = with pkgs; [
    terminus_font
  ];

  i18n = {
    consoleFont = "ter-m32n";
  };
}

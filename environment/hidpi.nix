{ config, pkgs, ... }:

{
  fonts.fonts = with pkgs; [
    terminus_font
  ];

  i18n = {
    consoleFont = "ter-m32n";
  };

  environment.variables = {
    # GDK 3 virtual pixel scaling
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";

    # Qt 5 virtual pixel scaling
    QT_AUTO_SCREEN_SCALE_FACTOR = "0";
    QT_SCREEN_SCALE_FACTORS = "2";
  };
}

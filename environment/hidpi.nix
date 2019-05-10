{ config, pkgs, ... }:

{
  i18n.consoleFont = "${pkgs.terminus_font}/share/consolefonts/ter-132n";
  services.xserver.dpi = 192;
  # fonts.fontconfig.dpi = 192;

  environment.variables = {
    # GDK 3+ virtual pixel scaling
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";

    # Qt 5+ virtual pixel scaling
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";

    # winit virtual pixel scaling
    WINIT_HIDPI_FACTOR = "2";
  };
}

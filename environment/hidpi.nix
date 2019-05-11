{ config, pkgs, ... }:

{
  i18n.consoleFont = "${pkgs.terminus_font}/share/consolefonts/ter-132n";
  services.xserver = {
    dpi = 192;
    displayManager.sessionCommands = ''
      # GDK 3+ virtual pixel scaling
      export GDK_SCALE=2
      export GDK_DPI_SCALE=0.5

      # Qt 5+ virtual pixel scaling
      export QT_AUTO_SCREEN_SCALE_FACTOR=1

      # winit virtual pixel scaling
      export WINIT_HIDPI_FACTOR=2
    '';
  };
}

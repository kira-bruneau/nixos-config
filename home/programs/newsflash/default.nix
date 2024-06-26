{ config, pkgsKiraNur, ... }:

let
  newsflash = pkgsKiraNur.newsflash;
in
{
  imports = [ ../../environments/config.nix ];

  home.packages = [ newsflash ];

  # Manage newsflash config outside of home-manager while keeping track of the files in this git repo
  xdg.configFile."news-flash/newsflash_gtk.json".source = config.lib.file.mkOutOfStoreSymlink "${config.home.configDirectory}/programs/newsflash/newsflash_gtk.json";

  wayland.windowManager.sway.config = {
    startup = [ { command = "${newsflash}/bin/io.gitlab.news_flash.NewsFlash"; } ];
    assigns."9" = [ { app_id = "^io.gitlab.news_flash.NewsFlash$"; } ];
  };
}

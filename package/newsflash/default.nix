{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    newsflash
  ];

  # Manage newsflash config outside of home-manager while keeping track of the files in this git repo
  xdg.configFile."news-flash/newsflash_gtk.json".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.configDirectory}/package/newsflash/newsflash_gtk.json";
}

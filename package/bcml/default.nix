{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    bcml
  ];

  # Manage bcml config outside of home-manager while keeping track of the files in this git repo
  xdg.configFile.bcml.source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.configDirectory}/package/bcml/config";
}

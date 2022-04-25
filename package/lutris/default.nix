{ config, pkgs, ... }:

{
  imports = [
    ../../environment/config.nix
  ];

  home.packages = with pkgs; [
    (lutris.override {
      lutris-unwrapped = lutris-unwrapped.override {
        wine = wineWowPackages.staging;
      };
    })
  ];

  # Manage lutris config outside of home-manager while keeping track of the files in this git repo
  xdg.configFile.lutris.source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.configDirectory}/package/lutris/config";
}

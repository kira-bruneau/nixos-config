{ config, pkgs, ... }:

{
  home.packages = with pkgs; with nur.repos.kira-bruneau; [
    (lutris.override { # unfreeRedistributable with steamSupport = true
      lutris-unwrapped = lutris-unwrapped.override {
        wine = wineWowPackages.staging;
      };
    })
  ];

  # Manage lutris config outside of home-manager while keeping track of the files in this git repo
  xdg.configFile.lutris.source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/.config/nixpkgs/package/lutris/config";
}

{ config, pkgs, ... }:

{
  home.packages = with pkgs; with nur.repos.kira-bruneau; [
    speedcrunch
  ];

  # Manage SpeedCrunch config outside of home-manager (synced with Syncthing)
  xdg.configFile.SpeedCrunch.source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/.config/nixpkgs/package/speedcrunch/config";
}

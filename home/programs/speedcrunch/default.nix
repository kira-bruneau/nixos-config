{ config, pkgs, ... }:

{
  imports = [
    ../../environments/config.nix
  ];

  home.packages = with pkgs; [
    speedcrunch
  ];

  # Manage SpeedCrunch config outside of home-manager (synced with Syncthing)
  xdg = {
    configFile.SpeedCrunch.source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.configDirectory}/programs/speedcrunch/config";

    dataFile.SpeedCrunch.source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.configDirectory}/programs/speedcrunch/share";
  };
}

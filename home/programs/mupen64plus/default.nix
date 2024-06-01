{ config, pkgs, ... }:

{
  imports = [ ../../environments/config.nix ];

  home.packages = with pkgs; [ mupen64plus ];

  xdg = {
    # TODO: Fix launching games with mupen64plus config
    configFile.mupen64plus.source = ./config;

    # TODO: Manage mupen64plus apps with lutris (requires upstream fix for rom paths with spaces)
    dataFile = {
      # Manage mupen64plus data directory outside of home-manager (synced with Syncthing)
      mupen64plus.source = config.lib.file.mkOutOfStoreSymlink "${config.home.configDirectory}/programs/mupen64plus/share/mupen64plus";

      "applications/mupen64plus".source = ./share/applications/mupen64plus;
      "icons/majoras-mask.png".source = ./share/icons/majoras-mask.png;
      "icons/MarioWingCap.png".source = ./share/icons/MarioWingCap.png;
      "icons/master-quest.png".source = ./share/icons/master-quest.png;
      "icons/ocarina.png".source = ./share/icons/ocarina.png;
    };
  };
}

{ config, pkgs, ... }:

{
  imports = [ ../../environments/config.nix ];

  home = {
    packages = with pkgs; [ lmms ];
    file = {
      # Manage lmms config outside of home-manager (synced with Syncthing)
      ".lmms".source = config.lib.file.mkOutOfStoreSymlink "${config.home.configDirectory}/programs/lmms/.lmms";
      ".lmmsrc.xml".source = config.lib.file.mkOutOfStoreSymlink "${config.home.configDirectory}/programs/lmms/.lmmsrc.xml";
    };
  };
}

{ config, pkgs, ... }:

{
  imports = [
    ../../environment/config.nix
  ];

  home = {
    packages = with pkgs; [
      lmms
    ];

    file = {
      # Manage lmms config outside of home-manager (synced with Syncthing)
      ".lmms".source = config.lib.file.mkOutOfStoreSymlink
        "${config.home.configDirectory}/package/lmms/.lmms";

      ".lmmsrc.xml".source = config.lib.file.mkOutOfStoreSymlink
        "${config.home.configDirectory}/package/lmms/.lmmsrc.xml";
    };
  };
}

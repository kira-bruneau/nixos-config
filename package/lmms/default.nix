{ config, pkgs, ... }:

{
  home = {
    packages = with pkgs; with nur.repos.kira-bruneau; [
      lmms
    ];

    file = {
      # Manage lmms config outside of home-manager (synced with Syncthing)
      ".lmms".source = config.lib.file.mkOutOfStoreSymlink
        "${config.home.homeDirectory}/.config/nixpkgs/package/lmms/.lmms";

      ".lmmsrc.xml".source = config.lib.file.mkOutOfStoreSymlink
        "${config.home.homeDirectory}/.config/nixpkgs/package/lmms/.lmmsrc.xml";
    };
  };
}

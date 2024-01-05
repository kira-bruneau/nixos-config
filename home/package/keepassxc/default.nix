{ config, pkgs, ... }:

{
  imports = [
    ../../environment/config.nix
  ];

  home.packages = with pkgs; [
    keepassxc
  ];

  # Manage keepassxc config outside of home-manager while keeping track of the files in this git repo
  xdg.configFile.keepassxc.source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.configDirectory}/package/keepassxc/config";

  wayland.windowManager.sway.config.startup = [
    { command = "${pkgs.keepassxc}/bin/keepassxc"; }
  ];
}

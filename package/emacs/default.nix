{ config, pkgs, ... }:

{
  imports = [
    ../../environment/config.nix
    ../gpg
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.callPackage ./package { };
  };

  home.sessionVariables = {
    EDITOR = "emacseditor";
  };

  # Manage emacs config outside of home-manager while keeping track of the files in this git repo
  xdg.configFile.emacs.source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.configDirectory}/package/emacs/config";
}

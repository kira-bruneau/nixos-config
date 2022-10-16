{ inputs, config, pkgs, ... }:

let
  emacsOverlayPkgs = inputs.emacs-overlay.overlays.default (pkgs // emacsOverlayPkgs) pkgs;
  callPackage = pkgs.newScope emacsOverlayPkgs;
in
{
  imports = [
    ../aspell
    ../gpg
  ];

  programs.emacs = {
    enable = true;
    package = callPackage ./package { };
  };

  home.sessionVariables = {
    EDITOR = "emacseditor";
  };

  # Manage emacs config outside of home-manager while keeping track of the files in this git repo
  xdg.configFile.emacs.source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.configDirectory}/package/emacs/config";
}

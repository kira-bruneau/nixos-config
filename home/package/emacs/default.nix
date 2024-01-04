{ config, pkgs, ... }:

let
  emacs = pkgs.callPackage ./package { };
in
{
  imports = [
    ../aspell
    ../gpg
  ];

  home = {
    packages = with pkgs; [
      emacs
    ];

    sessionVariables.EDITOR = "emacseditor";
  };

  # Manage emacs config outside of home-manager while keeping track of the files in this git repo
  xdg = {
    configFile.emacs.source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.configDirectory}/package/emacs/config";

    mimeApps.defaultApplications = {
      "text/plain" = "emacsclient.desktop";
      "inode/directory" = "emacsclient.desktop";
    };
  };

  wayland.windowManager.sway.config = {
    startup = [{ command = "${emacs}/bin/emacs"; }];
    assigns."2" = [{ app_id = "^emacs"; }];
  };
}

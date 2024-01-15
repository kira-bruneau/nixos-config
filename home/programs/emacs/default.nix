{ config, pkgs, ... }:

let
  emacs = pkgs.callPackage ./package { };
in
{
  imports = [
    ../../environments/config.nix
    ../aspell
    ../gpg
  ];

  home = {
    packages = with pkgs; [
      emacs
    ];

    sessionVariables.EDITOR = "emacseditor";
  };

  xdg = {
    configFile.emacs.source = ./config;

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

{ pkgs, pkgsKiraNur, ... }:

let
  emacs = pkgs.callPackage ./package.nix { ggt = pkgsKiraNur.ggt; };
in
{
  imports = [
    ../aspell
    ../gpg
  ];

  home = {
    packages = [ emacs ];
    sessionVariables.EDITOR = "emacseditor";
  };

  xdg.mimeApps.defaultApplications = {
    "text/plain" = "emacsclient.desktop";
  };

  wayland.windowManager.sway.config = {
    startup = [ { command = "${emacs}/bin/emacs"; } ];
    assigns."2" = [ { app_id = "^emacs"; } ];
  };
}

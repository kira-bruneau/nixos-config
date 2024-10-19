{ pkgs, ... }:

{
  imports = [
    ../aspell
    ../gpg
  ];

  home = {
    packages = [ pkgs.emacs ];
    sessionVariables.EDITOR = "emacseditor";
  };

  xdg.mimeApps.defaultApplications = {
    "text/plain" = "emacsclient.desktop";
  };

  wayland.windowManager.sway.config = {
    startup = [ { command = "emacs"; } ];
    assigns."2" = [ { app_id = "^emacs"; } ];
  };
}

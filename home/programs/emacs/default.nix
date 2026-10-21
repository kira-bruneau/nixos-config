{ config, pkgsCustom, ... }:

{
  imports = [
    ../aspell
    ../gpg
  ];

  services.emacs.package = pkgsCustom.memacs;

  home = {
    packages = [ config.services.emacs.package ];
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

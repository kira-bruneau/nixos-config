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

  programs.niri.settings = {
    spawn-at-startup = [ { command = [ "emacs" ]; } ];
    window-rules = [
      {
        matches = [ { app-id = "^emacs"; } ];
        open-on-workspace = "2-working";
        open-maximized = true;
      }
    ];
  };
}

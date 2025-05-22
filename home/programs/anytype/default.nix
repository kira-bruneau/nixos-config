{ pkgsKiraNur, ... }:

{
  home.packages = [ pkgsKiraNur.anytype ];

  xdg.configFile."anytype/devconfig.json".text = builtins.toJSON {
    sudo = true; # Disables analytics
    hideTray = true;
    showMenuBar = false;
    languages = [ "en-CA" ];
  };

  programs.niri.settings = {
    spawn-at-startup = [ { command = [ "anytype" ]; } ];
    window-rules = [
      {
        matches = [ { app-id = "^anytype$"; } ];
        open-on-workspace = "3-planning";
        open-maximized = true;
      }
    ];
  };
}

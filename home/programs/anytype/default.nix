{ pkgsKiraNur, ... }:

{
  home.packages = [ pkgsKiraNur.anytype ];

  xdg.configFile."anytype/devconfig.json".text = builtins.toJSON {
    sudo = true; # Disables analytics
    hideTray = true;
    showMenuBar = false;
    languages = [ "en-CA" ];
  };

  wayland.windowManager.sway.config = {
    startup = [ { command = "anytype"; } ];
    assigns."8" = [ { app_id = "^anytype$"; } ];
  };
}

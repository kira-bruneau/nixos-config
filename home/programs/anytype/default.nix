{ pkgsUnstable, pkgsKiraNur, ... }:

{
  home.packages = [ (pkgsKiraNur.anytype.override { electron = pkgsUnstable.electron_29; }) ];

  xdg.configFile."anytype/devconfig.json".text = builtins.toJSON {
    sudo = true; # Disables analytics
    hideTray = true;
    hideMenuBar = true;
  };

  wayland.windowManager.sway.config = {
    startup = [ { command = "anytype"; } ];
    assigns."8" = [ { app_id = "^anytype$"; } ];
  };
}

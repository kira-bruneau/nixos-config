{ ... }:

{
  services.picom = {
    enable = true;
    backend = "glx";
    shadow = true;
    shadowExclude = [
      "class_g = 'i3-frame'" # titlebars
      "class_g = 'Polybar'" # polybar tray
      "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'" # background windows in tabbed layout
      "name = 'cpt_frame_window'" # fix shadow overlaying zoom call
    ];
    opacityRules = [
      "90:name = 'Picture-in-Picture'"
    ];
    fade = true;
    fadeDelta = 10; # 100 steps per second
    fadeSteps = [ 0.0666 0.0444 ]; # ~150ms ~225ms
    settings = {
      clear-shadow = true;
      blur-background = true;
      no-fading-destroyed-argb = true;
    };
  };
}

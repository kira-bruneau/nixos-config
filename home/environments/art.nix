{ pkgs, pkgsKiraNur, ... }:

{
  imports = [ ../programs/lmms ];

  home.packages = with pkgs; [
    gimp
    inkscape
    krita
    orca-slicer
    tenacity
    yabridge
    yabridgectl
    pkgsKiraNur.zynaddsubfx
  ];

  wayland.windowManager.sway.config = {
    assigns."4" = [
      { app_id = "^org.inkscape.Inkscape$"; }
      { app_id = "^tenacity$"; }
      { class = "^Gimp"; }
      { class = "^krita$"; }
      { title = "^ZynAddSubFX"; }
    ];
  };
}

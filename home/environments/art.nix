{
  pkgs,
  pkgsKiraNur,
  ...
}:

{
  imports = [ ../programs/lmms ];

  home.packages = with pkgs; [
    gimp
    inkscape
    kdePackages.kdenlive
    krita
    orca-slicer
    tenacity
    pkgsKiraNur.yabridge
    pkgsKiraNur.yabridgectl
    pkgsKiraNur.zynaddsubfx
  ];

  wayland.windowManager.sway.config = {
    assigns."4" = [
      { app_id = "^org.inkscape.Inkscape$"; }
      { app_id = "^org.kde.kdenlive$"; }
      { app_id = "^tenacity$"; }
      { class = "^Gimp"; }
      { class = "^krita$"; }
      { title = "^ZynAddSubFX"; }
    ];
  };
}

{
  pkgs,
  pkgsKiraNur,
  pkgsYabridgeWine,
  ...
}:

{
  imports = [ ../programs/lmms ];

  home.packages = with pkgs; [
    gimp
    inkscape
    kdenlive
    krita
    orca-slicer
    tenacity
    (pkgsKiraNur.yabridge.override {
      wine = pkgsYabridgeWine.wineWowPackages.staging;
    })
    (pkgsKiraNur.yabridgectl.override {
      wine = pkgsYabridgeWine.wineWowPackages.staging;
    })
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

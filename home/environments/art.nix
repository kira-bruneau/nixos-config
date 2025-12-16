{
  pkgs,
  pkgsKiraNur,
  ...
}:

{
  imports = [
    ./dev/graphics.nix
    ../programs/lmms
  ];

  home.packages = with pkgs; [
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
      { app_id = "^org.kde.kdenlive$"; }
      { app_id = "^tenacity$"; }
      { class = "^krita$"; }
      { title = "^ZynAddSubFX"; }
    ];
  };
}

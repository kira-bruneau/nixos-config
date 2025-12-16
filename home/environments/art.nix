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
    # 2d
    krita

    # 3d
    orca-slicer

    # audio
    tenacity
    pkgsKiraNur.yabridge
    pkgsKiraNur.yabridgectl
    pkgsKiraNur.zynaddsubfx

    # video
    kdePackages.kdenlive
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

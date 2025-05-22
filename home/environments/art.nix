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

  programs.niri.settings = {
    window-rules = [
      {
        matches = [ { app-id = "^org.inkscape.Inkscape$"; } ];
        open-on-workspace = "2-working";
        open-maximized = true;
      }
      {
        matches = [ { app-id = "^org.kde.kdenlive$"; } ];
        open-on-workspace = "2-working";
        open-maximized = true;
      }
      {
        matches = [ { app-id = "^tenacity$"; } ];
        open-on-workspace = "2-working";
        open-maximized = true;
      }
      {
        matches = [ { app-id = "^Gimp"; } ];
        open-on-workspace = "2-working";
        open-maximized = true;
      }
      {
        matches = [ { app-id = "^krita$"; } ];
        open-on-workspace = "2-working";
        open-maximized = true;
      }
      {
        matches = [ { title = "^ZynAddSubFX"; } ];
        open-on-workspace = "2-working";
        open-floating = true;
      }
    ];
  };
}

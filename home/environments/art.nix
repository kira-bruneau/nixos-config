{ pkgs, pkgsKiraNur, ... }:

{
  imports = [ ../programs/lmms ];

  home.packages = with pkgs; [
    gimp
    inkscape
    krita
    tenacity
    yabridge
    yabridgectl
    pkgsKiraNur.zynaddsubfx
  ];
}

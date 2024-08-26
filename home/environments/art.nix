{ pkgs, pkgsKiraNur, ... }:

{
  imports = [ ../programs/lmms ];

  home.packages = with pkgs; [
    ffmpeg
    gimp
    inkscape
    krita
    tenacity
    yabridge
    yabridgectl
    pkgsKiraNur.zynaddsubfx
  ];
}

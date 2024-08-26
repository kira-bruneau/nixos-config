{ pkgs, pkgsKiraNur, ... }:

{
  imports = [ ../programs/lmms ];

  home.packages = with pkgs; [
    ffmpeg
    gimp
    inkscape
    tenacity
    yabridge
    yabridgectl
    pkgsKiraNur.zynaddsubfx
  ];
}

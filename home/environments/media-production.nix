{ pkgs, pkgsKiraNur, ... }:

{
  imports = [ ../programs/lmms ];

  home.packages = with pkgs; [
    audacity
    ffmpeg
    gimp
    inkscape
    yabridge
    yabridgectl
    pkgsKiraNur.zynaddsubfx
  ];
}

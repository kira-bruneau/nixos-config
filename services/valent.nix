{ pkgs, ... }:

{
  programs.kdeconnect = {
    enable = true;
    package = pkgs.valent;
  };
}

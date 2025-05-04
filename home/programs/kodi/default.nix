{ pkgs, ... }:

{
  programs.kodi = {
    enable = true;
    package = pkgs.kodi.withPackages (
      ps: with ps; [
        jellyfin
        youtube
      ]
    );
  };
}

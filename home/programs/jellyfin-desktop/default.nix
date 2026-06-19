{ pkgs, ... }:

let
  # Build jellyfin-desktop without libcec
  jellyfin-desktop = pkgs.jellyfin-desktop.overrideAttrs (attrs: {
    buildInputs = builtins.filter (p: p.pname != "libcec") attrs.buildInputs;
  });
in
{
  home.packages = [ jellyfin-desktop ];
  wayland.windowManager.sway.config.assigns."4" = [ { app_id = "^org.jellyfin.JellyfinDesktop$"; } ];
}

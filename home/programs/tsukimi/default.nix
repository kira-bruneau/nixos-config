{ pkgs, ... }:

{
  home.packages = with pkgs; [ tsukimi ];
  wayland.windowManager.sway.config.assigns."4" = [ { app_id = "^moe.tsuna.tsukimi$"; } ];
}

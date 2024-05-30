{ pkgs, ... }:

{
  home.packages = with pkgs; [
    (prismlauncher.override {
      withWaylandGLFW = true;
    })
  ];

  wayland.windowManager.sway.config.assigns."5" = [
    { app_id = "^org.prismlauncher.PrismLauncher$"; }
    { class = "Minecraft"; }
  ];
}

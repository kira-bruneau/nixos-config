{ pkgs, ... }:

{
  home.packages = with pkgs; [
    prismlauncher
  ];

  wayland.windowManager.sway.config.assigns."5" = [
    { app_id = "^org.prismlauncher.PrismLauncher$"; }
  ];
}

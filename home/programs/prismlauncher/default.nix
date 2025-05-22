{ pkgs, ... }:

{
  home.packages = with pkgs; [ prismlauncher ];

  programs.niri.settings = {
    window-rules = [
      {
        matches = [ { app-id = "^org.prismlauncher.PrismLauncher$"; } ];
        open-on-workspace = "5-gaming";
      }
      {
        matches = [ { app-id = "^Minecraft"; } ];
        open-on-workspace = "5-gaming";
        open-fullscreen = true;
      }
    ];
  };
}

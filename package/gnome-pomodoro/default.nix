{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gnome.pomodoro
  ];

  dconf.settings = {
    "org/gnome/pomodoro/preferences" = {
      enabled-plugins = ["sounds" "dark-theme"];
    };
  };
}

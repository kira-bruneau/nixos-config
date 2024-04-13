{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gnome.dconf-editor
  ];

  dconf.settings = {
    "ca/desrt/dconf-editor" = {
      show-warning = false;
    };
  };
}

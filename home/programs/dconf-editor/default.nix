{ pkgs, ... }:

{
  home.packages = with pkgs; [ dconf-editor ];

  dconf.settings = {
    "ca/desrt/dconf-editor" = {
      show-warning = false;
    };
  };
}

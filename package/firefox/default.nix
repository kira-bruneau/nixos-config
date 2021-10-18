{ pkgs, ... }:

{
  home.packages = with pkgs; [
    firefox-wayland
  ];

  xdg.dataFile = {
    "applications/firefox.desktop".source = ./firefox.desktop;
  };

  home.sessionVariables = {
    # Touchscreen support on Firefox
    MOZ_USE_XINPUT2 = "1";
  };
}

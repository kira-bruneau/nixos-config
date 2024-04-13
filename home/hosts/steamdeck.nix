{ inputs, config, lib, pkgs, ... }:

{
  imports = [
    ../environments/bluetooth.nix
    ../environments/gaming.nix
    ../environments/gui/gnome.nix
  ];

  home.stateVersion = "23.11";

  # Enable CEF remote debugging for decky-loader
  xdg.dataFile."Steam/.cef-enable-remote-debugging".text = "";

  dconf.settings = {
    # Enable on-screen keyboard
    "org/gnome/desktop/a11y/applications" = {
      screen-keyboard-enabled = true;
    };
  };

  xdg.desktopEntries = {
    youtube = {
      categories = [ "Network" "WebBrowser" ];
      exec = "firefox --new-window --kiosk https://www.youtube.com";
      genericName = "Web Browser";
      icon = pkgs.fetchurl {
        url = "https://upload.wikimedia.org/wikipedia/commons/0/09/YouTube_full-color_icon_%282017%29.svg";
        hash = "sha256-fROAuewbDLM/ZhsgM9E77KfETCxAiabRTElVX/4/Ir8=";
      };
      name = "Youtube";
      type = "Application";
    };

    dropout = {
      categories = [ "Network" "WebBrowser" ];
      exec = "firefox --new-window --kiosk https://www.dropout.tv";
      genericName = "Web Browser";
      icon = pkgs.fetchurl {
        url = "https://theme.zdassets.com/theme_assets/2371800/512ccab6375a880e06985eb98aea5acbb9359ba7.jpg";
        hash = "sha256-aSHPwfz5fhm5Ra9zJrK8Uxkz63UjIPl4nap3cWBAjIE=";
      };
      name = "Dropout";
      type = "Application";
    };
  };
}

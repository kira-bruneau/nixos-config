{ inputs, config, lib, pkgs, pkgsNixSoftwareCenter, ... }:

{
  imports = [
    ../environments/bluetooth.nix
    ../environments/gaming.nix
    ../environments/gui/gnome.nix
  ];

  home = {
    stateVersion = "23.11";
    packages = with pkgs; [
      pkgsNixSoftwareCenter.nix-software-center
    ];
  };

  # Enable CEF remote debugging for decky-loader
  xdg.dataFile."Steam/.cef-enable-remote-debugging".text = "";

  dconf.settings = {
    # Enable on-screen keyboard
    "org/gnome/desktop/a11y/applications" = {
      screen-keyboard-enabled = true;
  };

  # Add firefox profile for Jack
  programs.firefox.profiles.jackfox =
    let base = config.programs.firefox.profiles.firefox; in {
      id = base.id + 1;
      name = "Jackfox";
      path = "jack";
      settings = base.settings // {
        "extensions.activeThemeID" = "{d26a3404-d978-4bd6-93cf-f9749f57b923}";
        "services.sync.username" = "jack.loder@outlook.com";
      };
    };

  xdg.desktopEntries = {
    jackfox.icon = lib.mkForce (pkgs.fetchurl {
      url = "https://upload.wikimedia.org/wikipedia/commons/3/30/Firefox_Developer_Edition_logo%2C_2019.svg";
      hash = "sha256-gQk9Uz20oMJiA77HmlLp75VuwDudL64x7IPaz+PBca4=";
    });

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

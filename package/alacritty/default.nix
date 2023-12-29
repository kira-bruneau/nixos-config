{ config, ... }:

{
  programs.alacritty = {
    enable = true;
    settings = {
      window = {
        padding = {
          x = 10;
          y = 10;
        };
        opacity = 0.95;
      };
      font.size = 9.0;
      colors = {
        primary = {
          background = "0x212121";
          foreground = "0xc0c5ce";
          bright_foreground = "0xf3f4f5";
        };
        cursor = {
          text = "0x212121";
          cursor = "0xc0c5ce";
        };
        normal = {
          black = "0x212121";
          red = "0xe57373";
          green = "0xa6bc69";
          yellow = "0xfac863";
          blue = "0x6699cc";
          magenta = "0xc594c5";
          cyan = "0x5fb3b3";
          white = "0xc0c5ce";
        };
        bright = {
          black = "0x5c5c5c";
          red = "0xe57373";
          green = "0xa6bc69";
          yellow = "0xfac863";
          blue = "0x6699cc";
          magenta = "0xc594c5";
          cyan = "0x5fb3b3";
          white = "0xf3f4f5";
        };
      };

      scrolling.multiplier = 10;
    };
  };

  wayland.windowManager.sway.config.terminal = "${config.programs.alacritty.package}/bin/alacritty";
}

{ config, ... }:

{
  environment.etc."sway/config.d/colemak.conf" = {
    enable = config.programs.sway.enable;
    text = ''
      input * xkb_layout "us,us"
      input * xkb_variant "colemak,"
      input * xkb_options "grp:win_space_toggle"
    '';
  };

  console.keyMap = ./colemak.map;
}

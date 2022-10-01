{ lib, config, ... }:

{
  environment.etc = lib.mkIf config.programs.sway.enable {
    "sway/config.d/colemak.conf".text = ''
      input * xkb_layout "us,us"
      input * xkb_variant "colemak,"
      input * xkb_options "grp:win_space_toggle"
    '';
  };

  services.xserver = {
    layout = "us,us";
    xkbVariant = "colemak,";
    xkbOptions = "grp:win_space_toggle";
  };

  console.keyMap = ./colemak.map;
}

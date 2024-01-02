{ config, lib, ... }:

{
  console.keyMap = ./colemak.map;

  environment.etc."sway/config.d/colemak.conf".text = ''
    input * xkb_layout "us,us"
    input * xkb_variant "colemak,"
    input * xkb_options "grp:win_space_toggle"
  '';

  programs.dconf.profiles = lib.mkIf config.services.xserver.desktopManager.gnome.enable {
    user.databases = [
      {
        settings = with lib.gvariant; {
          "org/gnome/desktop/input-sources" = {
            sources = [
              (mkTuple [ "xkb" "us+colemak" ])
              (mkTuple [ "xkb" "us" ])
            ];
            xkb-options = mkEmptyArray type.string;
          };
        };
      }
    ];
  };

  # Fallback to initrd console configuration
  systemd.services = {
    systemd-vconsole-setup.enable = false;
    reload-systemd-vconsole-setup.enable = false;
  };
}

{ config, lib, ... }:

{
  environment.etc."sway/config.d/colemak.conf".text = ''
    input * xkb_layout "${config.services.xserver.xkb.layout}"
    input * xkb_variant "${config.services.xserver.xkb.variant}"
    input * xkb_options "${config.services.xserver.xkb.options}"
  '';

  programs.dconf.profiles = lib.mkIf config.services.xserver.desktopManager.gnome.enable {
    user.databases = [
      {
        settings = with lib.gvariant; {
          "org/gnome/desktop/input-sources" = {
            sources =
              lib.zipListsWith
                (
                  layout: variant:
                  (mkTuple [
                    "xkb"
                    "${layout}${lib.optionalString (variant != "") "+${variant}"}"
                  ])
                )
                (lib.splitString "," config.services.xserver.xkb.layout)
                (lib.splitString "," config.services.xserver.xkb.variant);
            xkb-options = lib.splitString "," config.services.xserver.xkb.options;
          };
        };
      }
    ];
  };
}

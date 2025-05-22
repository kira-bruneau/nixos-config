{ config, lib, ... }:

{
  programs.dconf.profiles = lib.mkIf config.services.xserver.desktopManager.gnome.enable {
    user.databases = [
      {
        settings = {
          "org/gnome/desktop/input-sources" = {
            sources =
              lib.zipListsWith
                (
                  layout: variant:
                  (lib.gvariant.mkTuple [
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

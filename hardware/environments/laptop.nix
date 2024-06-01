{
  lib,
  config,
  pkgs,
  ...
}:

let
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
in
{
  imports = [
    ./bluetooth.nix
    ./portable.nix
    ./wifi.nix
  ];

  # Automatically control frequency of CPU to save power
  services.auto-cpufreq.enable = true;

  # Disable tlp being enabled from common-pc-laptop in nixos-hardware
  services.tlp.enable = false;

  # Touchpad configuration
  environment.etc."sway/config.d/touchpad.conf".text = ''
    input type:touchpad {
      natural_scroll enabled
      tap enabled
      scroll_factor 0.25
    }
  '';

  programs.dconf.profiles = lib.mkIf config.services.xserver.desktopManager.gnome.enable {
    user.databases = [
      {
        settings = with lib.gvariant; {
          "org/gnome/desktop/peripherals/touchpad" = {
            natural-scroll = true;
          };
        };
      }
    ];
  };

  # Use brightnessctl for controlling backlight
  environment.etc."sway/config.d/backlight-controls.conf".text = ''
    bindsym XF86MonBrightnessUp exec ${brightnessctl} set 5%+
    bindsym XF86MonBrightnessDown exec ${brightnessctl} set 5%-
    bindsym Shift+XF86MonBrightnessUp exec ${brightnessctl} set 100%
    bindsym Shift+XF86MonBrightnessDown exec ${brightnessctl} set 1

    # using volume scroller (really nice with the Corsair Vengeance K95)
    bindsym Mod1+XF86AudioRaiseVolume exec ${brightnessctl} set 5%+
    bindsym Mod1+XF86AudioLowerVolume exec ${brightnessctl} set 5%-
    bindsym Mod1+Shift+XF86AudioRaiseVolume exec ${brightnessctl} set 100%
    bindsym Mod1+Shift+XF86AudioLowerVolume exec ${brightnessctl} set 1
  '';
}

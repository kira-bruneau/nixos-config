{ config, ... }:

{
  imports = [
    ./desktop.nix
    ./portable.nix
  ];

  # Power management
  services.upower.enable = true;
  systemd.services.upower.wantedBy = [ "multi-user.target" ];

  # Automatically control frequency of CPU to save power
  services.auto-cpufreq.enable = true;

  # Disable tlp being enabled from common-pc-laptop in nixos-hardware
  services.tlp.enable = false;

  # Enable touchpad support
  services.xserver.libinput = {
    enable = true;
    touchpad = {
      naturalScrolling = true;
      tapping = true;
    };
  };

  environment.etc."sway/config.d/touchpad.conf" = {
    enable = config.programs.sway.enable;
    text = ''
      input type:touchpad {
        natural_scroll enabled
        tap enabled
        scroll_factor 0.25
      }
    '';
  };

  # Enable light for controlling backlight
  programs.light.enable = true;
  environment.etc."sway/config.d/backlight-controls.conf" = {
    enable = config.programs.sway.enable;
    text = ''
      bindsym XF86MonBrightnessUp exec --no-startup-id light -A 10
      bindsym XF86MonBrightnessDown exec --no-startup-id light -U 10
      bindsym Shift+XF86MonBrightnessUp exec --no-startup-id light -S 100
      bindsym Shift+XF86MonBrightnessDown exec --no-startup-id light -r -S 1
      # using volume scroller (really nice with the Corsair Vengeance K95)
      bindsym Mod1+XF86AudioRaiseVolume exec --no-startup-id light -A 10
      bindsym Mod1+XF86AudioLowerVolume exec --no-startup-id light -U 10
      bindsym Mod1+Shift+XF86AudioRaiseVolume exec --no-startup-id light -S 100
      bindsym Mod1+Shift+XF86AudioLowerVolume exec --no-startup-id light -r -S 1
    '';
  };
}

{ config, pkgs, ... }:

{
  # Services
  services.xserver = {
    enable = true;
    useGlamor = true;
    displayManager = {
      lightdm.enable = true;
      defaultSession = "none+i3";
    };

    # This is the default behaviour for stateVersion >= 19.09
    desktopManager.xterm.enable = false;

    # Enable i3-gaps
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      extraPackages = [];
    };
  };

  # TODO: Start at login
  services.redshift.enable = true;
  location.provider = "geoclue2";

  programs.sway = {
    enable = true;
    extraPackages = with pkgs; [ swaylock swayidle xwayland ];
  };

  # Enable DConf
  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  # Disable bitmap fonts
  fonts.fontconfig.allowBitmaps = false;
}

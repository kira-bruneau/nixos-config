{ config, pkgs, ... }:

{
  imports = [ ./nur.nix ];

  # Services
  services.xserver = {
    enable = true;
    useGlamor = true;
    displayManager = {
      lightdm = {
        enable = true;
        greeters.webkit2 = {
          enable = true;
          webkitTheme = fetchTarball {
            url = "https://github.com/Litarvan/lightdm-webkit-theme-litarvan/releases/download/v3.0.0/lightdm-webkit-theme-litarvan-3.0.0.tar.gz";
            sha256 = "0q0r040vxg1nl51wb3z3r0pl7ymhyhp1lbn2ggg7v3pa563m4rrv";
          };
          branding.backgroundImages = "${pkgs.gnome3.gnome-backgrounds}/share/backgrounds/gnome";
        };
      };
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

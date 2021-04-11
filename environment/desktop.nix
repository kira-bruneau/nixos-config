{ config, pkgs, ... }:

{
  imports = [
    ./nur.nix
  ];

  environment = {
    # Add home bin to PATH
    homeBinInPath = true;

    # Source ~/.profile on login to properly set 'home.sessionVariables'
    # with home-manager.
    #
    # See https://github.com/rycee/home-manager/issues/1011
    loginShellInit = ''
      if [ -e $HOME/.profile ]; then
        . $HOME/.profile
      fi
    '';

    systemPackages = with pkgs; [
      sudo
    ];
  };

  # Enable LightDM display manager
  services.xserver = {
    enable = true;
    displayManager = {
      defaultSession = "sway";
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
    };
  };

  # Enable Sway window manager
  programs.sway = {
    enable = true;
    extraPackages = with pkgs; [ swaylock swayidle xwayland ];
    wrapperFeatures.gtk = true;
  };

  # Enable i3-gaps window manager
  services.xserver = {
    useGlamor = true;
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      extraPackages = [];
    };
  };

  # Disable xterm desktop manager
  # This is the default behaviour for stateVersion >= 19.09
  services.xserver.desktopManager.xterm.enable = false;

  # Redshift
  # TODO: Start at login
  location.provider = "geoclue2";
  services.redshift = {
    enable = true;
    package = pkgs.redshift-wlr;
  };

  # Enable pipewire (sound & video)
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    media-session.enable = true;

    alsa.enable = true;
    jack.enable = true;
    pulse.enable = true;
  };

  # Enable xdg-desktop-portal (screen sharing)
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-wlr ];
  };

  # Enable DConf & GNOME crypto services
  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ gcr gnome3.dconf ];

  # Disable bitmap fonts
  fonts.fontconfig.allowBitmaps = false;

  # Let the desktop environment handle the power key
  services.logind.extraConfig = "HandlePowerKey=ignore";
}

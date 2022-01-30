{ config, pkgs, ... }:

{
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
      (themes.sddm.clairvoyance.override {
        autoFocusPassword = true;
      })
    ];
  };

  # Enable LightDM display manager
  services.xserver = {
    enable = true;
    displayManager = {
      defaultSession = "sway";
      sddm = {
        enable = true;
        theme = "clairvoyance";
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

  # Redshift
  # TODO: Start at login
  location.provider = "geoclue2";
  services.redshift = {
    enable = true;
    package = pkgs.gammastep;
    executable = "/bin/gammastep";
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

  # Enable automatic device mounting
  services.devmon.enable = true;

  # Enable DConf
  programs.dconf.enable = true;

  # Enable GNOME crypto services
  services.dbus.packages = with pkgs; [ gcr ];

  # Disable bitmap fonts
  fonts.fontconfig.allowBitmaps = false;

  # Let the desktop environment handle the power key
  services.logind.extraConfig = "HandlePowerKey=ignore";
}

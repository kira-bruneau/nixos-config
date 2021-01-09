{ config, pkgs, ... }:

{
  imports = [ ./nur.nix ];

  # Services
  services.xserver = {
    enable = true;
    useGlamor = true;
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

    # This is the default behaviour for stateVersion >= 19.09
    desktopManager.xterm.enable = false;

    # Enable i3-gaps
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      extraPackages = [];
    };
  };

  # Source ~/.profile on login to properly set 'home.sessionVariables'
  # with home-manager.
  #
  # See https://github.com/rycee/home-manager/issues/1011
  environment.loginShellInit = ''
    if [ -e $HOME/.profile ]; then
      . $HOME/.profile
    fi
  '';

  # Redshift
  # TODO: Start at login
  services.redshift = {
    enable = true;
    package = pkgs.redshift-wlr;
  };

  location.provider = "geoclue2";

  # Enable sway
  programs.sway = {
    enable = true;
    extraPackages = with pkgs; [ swaylock swayidle xwayland ];
    wrapperFeatures.gtk = true;
  };

  # Enable xdg-desktop-portal (screen sharing)
  services.pipewire.enable = true;
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-wlr ];
  };

  # Enable flatpak
  services.flatpak.enable = true;

  # Enable DConf
  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  # Disable bitmap fonts
  fonts.fontconfig.allowBitmaps = false;

  # Enable debug symbols on sway & wlroots
  # https://github.com/swaywm/sway/wiki/Development-Setup#compiling-as-a-subproject
  nixpkgs.overlays = [
    (self: super:
      {
        sway-unwrapped = (super.enableDebugging super.sway-unwrapped).overrideAttrs (attrs: {
          mesonFlags = attrs.mesonFlags ++ [ "-Db_sanitize=address,undefined" ];
        });

        wlroots = (super.enableDebugging super.wlroots).overrideAttrs (attrs: {
          mesonFlags = attrs.mesonFlags ++ [ "-Db_sanitize=address,undefined" ];
        });
      })
  ];
}

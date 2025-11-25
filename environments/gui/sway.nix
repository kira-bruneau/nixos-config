{ pkgs, ... }:

{
  imports = [
    ./.
    ../../services/valent.nix
  ];

  environment = {
    systemPackages = with pkgs; [
      adwaita-icon-theme
      greetd.gtkgreet
    ];

    etc = {
      "sway/config.d/power-controls.conf".text = ''
        set $mode_power l̲ogout | s̲hutdown | r̲eboot
        mode "$mode_power" {
          bindsym l exec swaymsg exit
          bindsym s exec systemctl poweroff
          bindsym r exec systemctl reboot
          bindsym --release XF86PowerOff exec systemctl poweroff
          bindsym Control+Mod1+Delete exec systemctl reboot
          bindsym Return mode "default"
          bindsym Escape mode "default"
        }

        bindsym --release XF86PowerOff mode "$mode_power"
        bindsym Control+Mod1+Delete mode "$mode_power"
      '';

      "greetd/environments".text = ''
        sway
      '';
    };
  };

  # Enable Sway Wayland compositor
  programs.sway = {
    enable = true;
    package = pkgs.swayfx;
    extraPackages = [ ];
    wrapperFeatures.gtk = true;
  };

  security.pam.loginLimits = [
    {
      domain = "@users";
      item = "rtprio";
      type = "-";
      value = 1;
    }
  ];

  # Enable gtkgreet greeter (using Sway as the Wayland compositor)
  services.greetd = {
    enable = true;
    settings = {
      default_session.command =
        let
          background = pkgs.fetchurl {
            url = "https://cdn.artstation.com/p/assets/images/images/041/883/956/large/alena-aenami-wings-hd.jpg";
            hash = "sha256-7VcxN7ZpCaK07YpJFitN3fmWmNNIL0y+VW2E0agd3QU=";
          };

          # TODO: Update gtkgreet to use transparent background when no background is specified
          transparent = pkgs.fetchurl {
            url = "https://upload.wikimedia.org/wikipedia/commons/c/ca/1x1.png";
            hash = "sha256-PrEHktHwx+B+ckgnNUDxlS2aWimW9LXfcKsCbNnwVRc=";
          };

          style = pkgs.writeText "gtkgreet.css" ''
            * {
                color: #eee;
                text-shadow: 1px 1px 2px #233;
            }

            #clock {
                margin-bottom: 4px;
            }

            entry {
                color: #444;
                text-shadow: none;
                box-shadow: 1px 1px 2px #233;
                border-width: 2px;
                padding: 4px 8px;
                background-color: #eee;
            }

            button {
                box-shadow: 1px 1px 2px #233;
                border-width: 2px;
                padding: 8px;
                background-color: #eee;
            }

            button * {
                color: #444;
                text-shadow: none;
            }

            body {
                padding-bottom: 4px;
            }

            combobox button {
                box-shadow: none;
                padding: 4px;
            }

            button.suggested-action * {
                color: #eee;
            }
          '';

          gtkgreet-sway-config = pkgs.writeText "gtkgreet-sway-config" ''
            output * bg ${background} fill
            seat * xcursor_theme Adwaita 24
            exec "dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK XDG_CURRENT_DESKTOP; gtkgreet -l -b ${transparent} -s ${style}"
            include /etc/sway/config.d/*
          '';
        in
        "sway --config ${gtkgreet-sway-config}";
    };
  };

  # Redshift
  # TODO: Start at login
  services.redshift = {
    enable = true;
    package = pkgs.gammastep;
    executable = "/bin/gammastep";
  };

  # Enable pipewire (sound & video)
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    jack.enable = true;
    pulse.enable = true;
  };

  # Enable xdg-desktop-portal (screen sharing)
  xdg.portal = {
    wlr.enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  };

  # Enable GNOME virtual file system
  services.gvfs.enable = true;
  systemd.user.services = {
    gvfs-afc-volume-monitor.environment.GVFS_DEBUG = "1";
    gvfs-daemon.environment.GVFS_DEBUG = "1";
    gvfs-goa-volume-monitor.environment.GVFS_DEBUG = "1";
    gvfs-gphoto2-volume-monitor.environment.GVFS_DEBUG = "1";
    gvfs-metadata.environment.GVFS_DEBUG = "1";
    gvfs-mtp-volume-monitor.environment.GVFS_DEBUG = "1";
    gvfs-udisks2-volume-monitor.environment.GVFS_DEBUG = "1";
  };

  # Enable GNOME password manager
  services.gnome.gnome-keyring.enable = true;
  security.pam.services.greetd.enableGnomeKeyring = true;
  programs.seahorse.enable = true;

  # Disable fingerprint auth in greetd & swaylock
  security.pam.services.greetd.fprintAuth = false;
  security.pam.services.swaylock.fprintAuth = false;
}

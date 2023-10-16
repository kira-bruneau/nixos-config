{ lib, config, pkgs, ... }:

{
  environment = {
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
      xdg-utils
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

      # TODO: Add X + i3
      "greetd/environments".text = ''
        sway
      '';
    };
  };

  # Quiet boot
  # FIXME: This still shows fsck messages & NixOS messages from stage-1-init.sh & stage-2-init.sh scripts
  boot = {
    initrd.verbose = false;
    consoleLogLevel = 3;
    kernelParams = [
      "quiet"
      "rd.udev.log_level=3"
    ];
  };

  # Let the desktop environment handle the power key
  services.logind.extraConfig = "HandlePowerKey=ignore";

  # Enable Sway Wayland compositor
  programs.sway.enable = true;

  # Enable i3-gaps X11 window manager
  services.xserver.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    extraPackages = [];
  };

  # Enable gtkgreet greeter (using Sway as the Wayland compositor)
  services.greetd = {
    enable = true;
    settings = {
      default_session.command =
        let
          background = pkgs.fetchurl {
            url = "https://cdna.artstation.com/p/assets/images/images/041/883/956/large/alena-aenami-wings-hd.jpg";
            sha256 = "sha256-Taa/mlQ9wN5duE49jbXnLQXu5N9GiQQoew4F0wMFCTA=";
          };

          # TODO: Update gtkgreet to use transparent background when no background is specified
          transparent = pkgs.fetchurl {
            url = "https://upload.wikimedia.org/wikipedia/commons/c/ca/1x1.png";
            sha256 = "sha256-PrEHktHwx+B+ckgnNUDxlS2aWimW9LXfcKsCbNnwVRc=";
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
            exec "${pkgs.greetd.gtkgreet}/bin/gtkgreet -l -b ${transparent} -s ${style}; ${pkgs.sway}/bin/swaymsg exit"
            include /etc/sway/config.d/*
          '';
        in "sway --config ${gtkgreet-sway-config}";
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

  # Enable GUI for managing bluetooth
  services.blueman.enable = config.hardware.bluetooth.enable;

  # Enable xdg-desktop-portal (screen sharing)
  xdg.portal = {
    wlr.enable = true;

    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
    ];
  };

  # Enable automatic device mounting
  services.devmon.enable = true;

  # Enable DConf
  programs.dconf.enable = true;

  # Enable GNOME crypto services
  services.dbus.packages = with pkgs; [ gcr ];

  # Enable GNOME virtual file system
  services.gvfs.enable = true;

  # Enable GNOME password manager
  services.gnome.gnome-keyring.enable = true;
  security.pam.services.greetd.enableGnomeKeyring = true;

  # Disable bitmap fonts
  fonts.fontconfig.allowBitmaps = false;

  # Reduce priority of nix daemon on desktop systems so the system is
  # still usable while a nix build is running
  nix = {
    daemonCPUSchedPolicy = "idle";
    daemonIOSchedClass = "idle";
  };
}

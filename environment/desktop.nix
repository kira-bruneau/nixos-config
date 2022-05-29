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
  };

  # Enable Sway Wayland compositor
  programs.sway = {
    enable = true;
    extraPackages = with pkgs; [ swaylock swayidle ];
    wrapperFeatures.gtk = true;
  };

  # Enable i3-gaps X11 window manager
  services.xserver = {
    useGlamor = true;
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      extraPackages = [];
    };
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
            output "Goldstar Company Ltd LG HDR 4K 0x0000B721" scale 2 pos 1712,1003
            output "Unknown 0x095F 0x00000000" scale 1.5 pos 208,1080
            output "Unknown HP Z27k G3 CN41223C6P" scale 2 pos 0 0

            input * xkb_layout "us,us"
            input * xkb_variant "colemak,"
            input * xkb_options "grp:win_space_toggle"

            bindsym XF86MonBrightnessUp exec --no-startup-id light -A 10
            bindsym XF86MonBrightnessDown exec --no-startup-id light -U 10
            bindsym Shift+XF86MonBrightnessUp exec --no-startup-id light -S 100
            bindsym Shift+XF86MonBrightnessDown exec --no-startup-id light -r -S 1
            # using volume scroller (really nice with the Corsair Vengeance K95)
            bindsym Mod1+XF86AudioRaiseVolume exec --no-startup-id light -A 10
            bindsym Mod1+XF86AudioLowerVolume exec --no-startup-id light -U 10
            bindsym Mod1+Shift+XF86AudioRaiseVolume exec --no-startup-id light -S 100
            bindsym Mod1+Shift+XF86AudioLowerVolume exec --no-startup-id light -r -S 1

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

            exec "${pkgs.greetd.gtkgreet}/bin/gtkgreet -l -b ${transparent} -s ${style}; ${pkgs.sway}/bin/swaymsg exit"
            include /etc/sway/config.d/*
          '';
        in "sway --config ${gtkgreet-sway-config}";
    };
  };

  environment.systemPackages = [
    (pkgs.writeShellScriptBin "wayland-session" ''
      /run/current-system/systemd/bin/systemctl --user start graphical-session.target
      "$@"
      /run/current-system/systemd/bin/systemctl --user stop graphical-session.target
    '')
  ];

  # TODO: Add X + i3
  environment.etc."greetd/environments".text = ''
    wayland-session sway
  '';

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
}

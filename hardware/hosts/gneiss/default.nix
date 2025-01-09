{
  inputs,
  config,
  lib,
  pkgsKiraNur,
  modulesPath,
  ...
}:

{
  imports = [
    (modulesPath + "/installer/sd-card/sd-image-aarch64.nix")
    (inputs.nixpkgs-unstable + "/nixos/modules/services/misc/klipper.nix")
  ];

  disabledModules = [
    "services/misc/klipper.nix"
  ];

  fileSystems = {
    "/" = lib.mkForce {
      device = "none";
      fsType = "tmpfs";
      options = [
        "defaults"
        "size=25%"
        "mode=755"
      ];
    };

    "/persist" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      neededForBoot = true;
      fsType = "ext4";
    };
  };

  environment.persistence."/persist".directories = [
    "/boot"
    "/home"
    "/nix"
  ];

  services.klipper = {
    enable = true;
    package = pkgsKiraNur.ender3-v3-se-klipper-with-display;

    octoprintIntegration = true;

    settings = {
      "include /etc/klipper.d/*.cfg" = { };

      virtual_sdcard = {
        path = "/var/lib/moonraker/gcodes";
        on_error_gcode = "CANCEL_PRINT";
      };

      # input_shaper = {
      #   shaper_type_y = "mzv";
      #   shaper_freq_y = 35.0;
      # };

      stepper_x = {
        step_pin = "PC2";
        dir_pin = "!PB9";
        enable_pin = "!PC3";
        microsteps = 64;
        rotation_distance = 40;
        endstop_pin = "!PA5";
        position_endstop = -9;
        position_min = -9;
        position_max = 228;
        homing_speed = 50;
      };

      stepper_y = {
        step_pin = "PB8";
        dir_pin = "PB7";
        enable_pin = "!PC3";
        microsteps = 64;
        rotation_distance = 40;
        endstop_pin = "!PA6";
        position_endstop = -13;
        position_min = -13;
        position_max = 230;
        homing_speed = 50;
      };

      stepper_z = {
        step_pin = "PB6";
        dir_pin = "!PB5";
        enable_pin = "!PC3";
        microsteps = 16;
        rotation_distance = 8;
        endstop_pin = "probe:z_virtual_endstop";
        position_min = -3;
        position_max = 250;
        homing_speed = 4;
        second_homing_speed = 1;
        homing_retract_dist = 2.0;
      };

      extruder = {
        max_extrude_only_distance = 100.0;
        pressure_advance = 4.0e-2;
        step_pin = "PB4";
        dir_pin = "PB3";
        enable_pin = "!PC3";
        microsteps = 16;
        rotation_distance = 7.44;
        nozzle_diameter = 0.4;
        filament_diameter = 1.75;
        heater_pin = "PA1";
        sensor_type = "EPCOS 100K B57560G104F";
        sensor_pin = "PC5";
        min_temp = 0;
        max_temp = 260;
      };

      heater_bed = {
        heater_pin = "PB2";
        sensor_type = "EPCOS 100K B57560G104F";
        sensor_pin = "PC4";
        min_temp = 0;
        max_temp = 100;
      };

      "heater_fan hotend_fan" = {
        pin = "PC1";
        max_power = 1.0;
        shutdown_speed = 0;
        cycle_time = 1.0e-2;
        hardware_pwm = false;
        kick_start_time = 0.1;
        off_below = 0.0;
        heater = "extruder";
        heater_temp = 50;
        fan_speed = 1.0;
      };

      fan.pin = "PA0";

      mcu = {
        serial = "/dev/serial/by-id/usb-1a86_USB_Serial-if00-port0";
        restart_method = "command";
      };

      printer = {
        kinematics = "cartesian";
        max_velocity = 150;
        max_accel = 3000;
        max_z_velocity = 50;
        square_corner_velocity = 5.0;
        max_z_accel = 500;
      };

      bltouch = {
        sensor_pin = "^PC14";
        control_pin = "PC13";
        x_offset = -24.0;
        y_offset = -15.5;
        speed = 20;
        pin_move_time = 0.4;
        stow_on_each_sample = false;
        probe_with_touch_mode = true;
      };

      safe_z_home = {
        home_xy_position = "115,115";
        speed = 50;
        z_hop = 10;
        z_hop_speed = 5;
      };

      bed_mesh = {
        speed = 120;
        horizontal_move_z = 5;
        mesh_min = "30,30"; # Need to handle head distance with cr-touch (bl_touch);
        mesh_max = "200,200";
        probe_count = "5,5";
        algorithm = "bicubic";
      };

      "output_pin beeper".pin = "PB0";

      "tmc2209 stepper_x" = {
        uart_pin = "PB12";
        run_current = 0.6;
        sense_resistor = 0.15;
        stealthchop_threshold = 0;
        interpolate = false;
      };

      "tmc2209 stepper_y" = {
        uart_pin = "PB13";
        run_current = 0.6;
        sense_resistor = 0.15;
        stealthchop_threshold = 0;
        interpolate = false;
      };

      "tmc2209 stepper_z" = {
        uart_pin = "PB14";
        run_current = 0.8;
        sense_resistor = 0.15;
        stealthchop_threshold = 0;
        interpolate = false;
      };

      "temperature_sensor MCU" = {
        sensor_type = "temperature_mcu";
        min_temp = 0;
        max_temp = 100;
      };

      "temperature_sensor ${config.networking.hostName}" = {
        sensor_type = "temperature_host";
        min_temp = 10;
        max_temp = 100;
      };

      pause_resume.recover_velocity = 25;

      display_status = { };

      respond = { };

      "delayed_gcode DELAYED_PRINTER_OFF" = {
        initial_duration = 0.0;
        gcode = "
          {% if printer.idle_timeout.state == \"Idle\" %}
            {% if printer[\"extruder\"].temperature > 50 %}
              UPDATE_DELAYED_GCODE ID=DELAYED_PRINTER_OFF DURATION=60
            {% else %}
              # WLED_OFF STRIP=roof
              POWER_OFF_PRINTER
            {% endif %}
          {% else %}
            M118 Printer not idle, cancelled PRINTER_OFF.
          {% endif %}
        ";
      };

      idle_timeout.gcode = "
        M84
        TURN_OFF_HEATERS
        UPDATE_DELAYED_GCODE ID=DELAYED_PRINTER_OFF DURATION=60
      ";

      "gcode_macro PROBE_ZOFFSET".gcode = "
        PRTOUCH_PROBE_ZOFFSET
        SAVE_CONFIG
      ";

      e3v3se_display = {
        language = "english";
        logging = true;
      };
    };

    firmwares = {
      mcu = {
        enable = true;
        configFile = ./ender3-v3-se.cfg;
      };
    };
  };

  environment.etc = {
    "klipper.d/macro.cfg".source = "${pkgsKiraNur.ender3-v3-se-klipper-config}/macro.cfg";
    "klipper.d/prtouch.cfg".source = "${pkgsKiraNur.ender3-v3-se-klipper-config}/prtouch.cfg";
  };

  services.octoprint.enable = true;

  services.fluidd = {
    enable = true;
    hostName = "fluidd.jakira.space";
  };

  services.mainsail = {
    enable = true;
    hostName = "mainsail.jakira.space";
  };

  services.moonraker = {
    enable = true;
    address = "0.0.0.0";
    allowSystemControl = true;
    settings = {
      octoprint_compat = { };

      authorization = {
        cors_domains = [
          "http://${config.services.fluidd.hostName}"
          "http://${config.services.mainsail.hostName}"
        ];

        trusted_clients = [
          "127.0.0.1"
          "100.64.0.0/10" # tailscale
        ];
      };
    };
  };

  systemd.services.moonraker.restartTriggers = [
    config.environment.etc."moonraker.cfg".source
  ];

  users.users.moonraker.extraGroups = [ "octoprint" ];

  security.polkit.enable = true;

  services.nginx = {
    enable = true;
    virtualHosts = {
      "octoprint.jakira.space" = {
        locations."/" = {
          proxyPass = "http://${
            if config.services.octoprint.host != null then config.services.octoprint.host else "127.0.0.1"
          }:${toString config.services.octoprint.port}";

          recommendedProxySettings = true;
          proxyWebsockets = true;
        };

        extraConfig = ''
          client_max_body_size 50M;
        '';
      };

      "fluidd.jakira.space".extraConfig = ''
        client_max_body_size 50M;
      '';

      "mainsail.jakira.space".extraConfig = ''
        client_max_body_size 50M;
      '';
    };
  };
}

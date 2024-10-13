{
  inputs,
  config,
  pkgs,
  ...
}:

{
  imports = [
    ../../drivers/logitech-wireless.nix
    ../../environments/laptop.nix
  ] ++ (with inputs.nixos-hardware.nixosModules; [ framework-11th-gen-intel ]);

  hardware = {
    enableRedistributableFirmware = false;
    cpu.intel.updateMicrocode = true;
    firmware = with pkgs; [
      # Display Microcontroller firmware:
      # i915/tgl_dmc_ver2_12.bin
      #
      # Bluetooth firmware:
      # intel/ibt-0041-0041.sfi
      #
      # Wifi firmware:
      # rtl_bt/iwlwifi-ty-a0-gf-a0-x.ucode
      linux-firmware
    ];

    # Support MangoHud GPU utilization
    intel-gpu-tools.enable = true;
  };

  boot = {
    # Use the systemd-boot EFI boot loader
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      timeout = 0;
    };

    kernelPackages = pkgs.linuxPackages_latest;
    resumeDevice = config.fileSystems."/persist".device or "";
    kernelParams = [ "resume_offset=5601280" ]; # sudo filefrag -v /persist/swapfile | awk 'NR==4 {print $4}' | sed 's/\.\.$//'
    kernel.sysctl = {
      "vm.swappiness" = 1;
    };
  };

  disko.devices = {
    disk.main = {
      device = "/dev/disk/by-id/nvme-WDS100T1X0E-00AFY0_215222803209";
      content = {
        type = "gpt";
        partitions = {
          boot = {
            type = "EF00";
            label = "boot";
            start = "1M";
            end = "513M";
            content = {
              type = "filesystem";
              format = "vfat";
              extraArgs = [
                "-F"
                "32"
                "-n"
                "boot"
              ];
              mountpoint = "/boot";
              mountOptions = [ "noatime" ];
            };
          };
          nixos = {
            label = "nixos";
            size = "100%";
            content = {
              type = "filesystem";
              format = "ext4";
              extraArgs = [
                "-L"
                "nixos"
              ];
              mountpoint = "/persist";
              mountOptions = [ "noatime" ];
            };
          };
        };
      };
    };

    nodev = {
      "/" = {
        fsType = "tmpfs";
        mountOptions = [
          "defaults"
          "mode=755"
        ];
      };
    };
  };

  environment.persistence."/persist".directories = [
    "/home"
    "/nix"
  ];

  # Hibernation swapfile
  swapDevices = [
    {
      device = "/persist/swapfile";
      size = 64102;
    }
  ];

  # Compress hibernation image as much as possible
  systemd.tmpfiles.rules = [ "w /sys/power/image_size - - - - 0" ];

  # Sway I/O configuration
  environment.etc."sway/config.d/io.conf".text = ''
    input "1133:16461:Logitech_K400_Plus" {
      natural_scroll enabled
      scroll_factor 0.25
    }

    output "BOE 0x095F Unknown" scale 1.5 pos 208 1080
    output "LG Electronics LG HDR 4K 0x0005B621" scale 2 pos 1712 1002
    output "Technical Concepts Ltd 65S535CA Unknown" scale 2 pos 0 0
  '';

  programs.captive-browser.interface = "wlan0";

  # WirePlumber device configuration
  services.pipewire.wireplumber.extraConfig = {
    alsa = {
      "monitor.alsa.rules" = [
        {
          matches = [ { "device.name" = "alsa_card.pci-0000_00_1f.3"; } ];
          actions = {
            update-props = {
              "device.nick" = "System";
              "device.description" = "System";
            };
          };
        }
        {
          matches = [ { "node.name" = "alsa_output.pci-0000_00_1f.3.hdmi-stereo"; } ];
          actions = {
            update-props = {
              "node.nick" = "Speaker";
              "node.description" = "Speaker";
            };
          };
        }
        {
          matches = [ { "node.name" = "alsa_input.pci-0000_00_1f.3.analog-stereo.2"; } ];
          actions = {
            update-props = {
              "node.nick" = "Microphone";
              "node.description" = "Microphone";
            };
          };
        }
      ];
    };
  };

  # Hibernate on low power
  services.udev.extraRules = ''
    SUBSYSTEM=="power_supply", ATTR{status}=="Discharging", ATTR{capacity}=="[0-4]", RUN+="/run/current-system/systemd/bin/systemctl hibernate"
  '';

  # Prevent CPU from overheating
  services.thermald.enable = true;

  # Manage firmware updates
  services.fwupd.enable = true;
}

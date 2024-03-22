{ inputs, config, pkgs, ... }:

{
  imports = [
    ./generated.nix
    ../../environments/laptop.nix
  ] ++ (with inputs.nixos-hardware.nixosModules; [
    framework-11th-gen-intel
  ]);

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
    kernel.sysctl = { "vm.swappiness" = 1; };
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
              extraArgs = [ "-F" "32" "-n" "boot" ];
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
              extraArgs = [ "-L" "nixos" ];
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
        mountOptions = [ "defaults" "mode=755" ];
      };
    };
  };

  # Hibernation swapfile
  swapDevices = [{ device = "/persist/swapfile"; size = 64102; }];

  # Compress hibernation image as much as possible
  systemd.tmpfiles.rules = [ "w /sys/power/image_size - - - - 0" ];

  # Sway I/O configuration
  environment.etc."sway/config.d/io.conf".text = ''
    input "1133:16461:Logitech_K400_Plus" {
      natural_scroll enabled
      scroll_factor 0.25
    }

    output "BOE 0x095F Unknown" scale 1.5 pos 0 77
    output "LG Electronics LG HDR 4K 0x0000B721" scale 2 pos 1504 0
    output "Technical Concepts Ltd 65S535CA Unknown" scale 2 pos -1920 0
  '';

  systemd.network.networks.wlan0 = {
    matchConfig.Name = "wlan0";
    networkConfig.DHCP = "yes";
  };

  # WirePlumber device configuration
  environment.etc."wireplumber/main.lua.d/51-config.lua".text = ''
    table.insert(alsa_monitor.rules, {
      matches = {
        {
          { "device.name", "equals", "alsa_card.pci-0000_00_1f.3" },
        },
      },
      apply_properties = {
        ["device.nick"] = "System",
        ["device.description"] = "System",
      },
    })

    table.insert(alsa_monitor.rules, {
      matches = {
        {
          { "node.name", "equals", "alsa_output.pci-0000_00_1f.3.analog-stereo" },
        },
      },
      apply_properties = {
        ["node.nick"] = "Speaker",
        ["node.description"] = "Speaker",
      },
    })

    table.insert(alsa_monitor.rules, {
      matches = {
        {
          { "node.name", "equals", "alsa_input.pci-0000_00_1f.3.analog-stereo" },
        },
      },
      apply_properties = {
        ["node.nick"] = "Microphone",
        ["node.description"] = "Microphone",
      },
    })

    table.insert(libcamera_monitor.rules, {
      matches = {
        {
          { "device.name", "equals", "libcamera_device.0" },
        },
      },
      apply_properties = {
         ["device.nick"] = "Camera",
         ["device.description"] = "Camera",
      },
    })

    table.insert(libcamera_monitor.rules, {
      matches = {
        {
          { "node.name", "equals", "libcamera_input.__SB_.PC00.XHCI.RHUB.HS07-7_1.0-0bda_5634" },
        },
      },
      apply_properties = {
         ["node.nick"] = "Camera",
         ["node.description"] = "Camera",
      },
    })

    v4l2_monitor.enabled = false
  '';

  # Hibernate on low power
  services.udev.extraRules = ''
    SUBSYSTEM=="power_supply", ATTR{status}=="Discharging", ATTR{capacity}=="[0-4]", RUN+="/run/current-system/systemd/bin/systemctl hibernate"
  '';

  # Automatically control frequency of CPU to save power
  services.auto-cpufreq.enable = true;

  # Disable tlp being enabled from common-pc-laptop in nixos-hardware
  services.tlp.enable = false;

  # Prevent CPU from overheating
  services.thermald.enable = true;

  # Manage firmware updates
  services.fwupd.enable = true;

  # Manage logitech unifying receiver
  hardware.logitech.wireless = {
    enable = true;
    enableGraphical = true;
  };
}

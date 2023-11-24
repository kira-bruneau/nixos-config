{ inputs, config, pkgs, ... }:

{
  imports = (with inputs.nixos-hardware.nixosModules; [
    framework
  ]) ++ [
    ../../environment/distributed-nix.nix
    ../../environment/laptop.nix
    ../../environment/locale
    ../../service/dnscrypt.nix
    ../../service/ssh.nix
    ../../user/kira.nix
  ];

  system.stateVersion = "22.11";

  boot = {
    # Use the systemd-boot EFI boot loader
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    kernelPackages = pkgs.linuxPackages_latest;
    resumeDevice = config.fileSystems."/".device;
    kernelParams = [ "resume_offset=22532096" ]; # sudo filefrag -v /swapfile | awk 'NR==4 {print $4}' | sed 's/\.\.$//'
    kernel.sysctl = { "vm.swappiness" = 1; };
  };

  disko.devices = {
    disk.main = {
      device = "/dev/disk/by-id/nvme-eui.e8238fa6bf530001001b448b45507c59";
      content = {
        type = "table";
        format = "gpt";
        partitions = [
          {
            name = "boot";
            start = "1MiB";
            end = "513MiB";
            bootable = true;
            content = {
              type = "filesystem";
              format = "vfat";
              extraArgs = [ "-F" "32" "-n" "boot" ];
              mountpoint = "/boot";
              mountOptions = [ "noatime" ];
            };
          }
          {
            name = "nixos";
            start = "513MiB";
            end = "100%";
            content = {
              type = "filesystem";
              format = "ext4";
              extraArgs = [ "-L" "nixos" ];
              mountpoint = "/";
              mountOptions = [ "noatime" ];
            };
          }
        ];
      };
    };

    nodev = {
      "/tmp" = {
        fsType = "tmpfs";
      };
    };
  };

  # Hibernation swapfile
  swapDevices = [ { device = "/swapfile"; size = 64102; }];

  # Compress hibernation image as much as possible
  systemd.tmpfiles.rules = [ "w /sys/power/image_size - - - - 0" ];

  # Sway output configuration
  environment.etc."sway/config.d/output.conf".text = ''
    output "BOE 0x095F Unknown" scale 1.5 pos 0 77
    output "LG Electronics LG HDR 4K 0x0000B721" scale 2 pos 1504 0
    output "Technical Concepts Ltd 65S535CA Unknown" scale 2 pos -1920 0
  '';

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

  # Prevent CPU from overheating
  services.thermald.enable = true;

  # Manage firmware updates
  services.fwupd.enable = true;
}

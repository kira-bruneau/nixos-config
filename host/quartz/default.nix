{ inputs, pkgs, ... }:

{
  imports = (with inputs.nixos-hardware.nixosModules; [
    common-cpu-amd
    common-gpu-amd
    common-pc-ssd
  ]) ++ [
    ../../environment/bluetooth.nix
    ../../environment/desktop.nix
    ../../environment/gaming.nix
    ../../service/kubo.nix
    ../../user/builder.nix
    ../../user/kira.nix
  ];

  system.stateVersion = "22.11";

  hardware.firmware = with pkgs; [
    rtl8761b-firmware
  ];

  boot = {
    # Use the systemd-boot EFI boot loader
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    kernelPackages = pkgs.linuxPackages_latest;
  };

  disko.devices = {
    disk = {
      main = {
        device = "/dev/disk/by-id/nvme-Corsair_MP600_CORE_212379080001303746AE";
        content = {
          type = "table";
          format = "gpt";
          partitions = [
            {
              name = "boot";
              start = "1MiB";
              end = "512MiB";
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
              start = "512MiB";
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

      media = {
        device = "/dev/disk/by-id/nvme-CT1000P1SSD8_1911E1F0AAC7";
        content = {
          type = "table";
          format = "gpt";
          partitions = [
            {
              name = "media";
              start = "0%";
              end = "100%";
              content = {
                type = "filesystem";
                format = "btrfs";
                mountpoint = "/srv";
              };
            }
          ];
        };
      };
    };

    nodev = {
      "/tmp" = {
        fsType = "tmpfs";
      };
    };
  };

  # Sway I/O configuration
  environment.etc."sway/config.d/io.conf".text = ''
    output "LG Electronics LG HDR 4K 0x0000B721" scale 2 pos 0,0
  '';

  systemd.network.networks.enp7s0 = {
    matchConfig.Name = "enp7s0";
    networkConfig.DHCP = "yes";
  };

  # WirePlumber device configuration
  environment.etc."wireplumber/main.lua.d/51-config.lua".text = ''
    table.insert(alsa_monitor.rules, {
      matches = {
        {
          { "device.name", "equals", "alsa_card.usb-Generic_USB_Condenser_Microphone_201701110001-00" },
        },
      },
      apply_properties = {
        ["device.nick"] = "Microphone",
        ["device.description"] = "Microphone",
      },
    })

    table.insert(alsa_monitor.rules, {
      matches = {
        {
          { "node.name", "equals", "alsa_input.usb-Generic_USB_Condenser_Microphone_201701110001-00.analog-stereo" },
        },
      },
      apply_properties = {
        ["node.nick"] = "Microphone",
        ["node.description"] = "Microphone",
      },
    })

    table.insert(alsa_monitor.rules, {
      matches = {
        {
          -- RX 7900 XTX HDMI output
          { "device.name", "equals", "alsa_card.pci-0000_0b_00.1" },
        },
        {
          -- RX 590 HDMI output
          { "device.name", "equals", "alsa_card.pci-0000_0c_00.1" },
        },
        {
          -- Analog audio jacks
          { "device.name", "equals", "alsa_card.pci-0000_0e_00.3" },
        },
      },
      apply_properties = {
        ["device.disabled"] = true,
      },
    })
  '';

  # Configure GPU optimisations for gamemode
  programs.gamemode.settings.gpu = {
    apply_gpu_optimisations = "accept-responsibility";
    gpu_device = 0;
    amd_performance_level = "high";
  };

  # Android debugging
  programs.adb.enable = true;

  containers.media-server = {
    autoStart = true;

    bindMounts = {
      "/srv" = {
        hostPath = "/srv";
        isReadOnly = false;
      };
    };

    config = {
      imports = [ ../../environment/media-server.nix ];
      system.stateVersion = "22.11";
      fonts.fontconfig.enable = false;
    };
  };

  networking.firewall.allowedTCPPorts = [
    8096
    5055
  ];
}

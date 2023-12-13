{ inputs, pkgs, ... }:

{
  imports = (with inputs.nixos-hardware.nixosModules; [
    common-cpu-amd
    common-gpu-amd
    common-pc-ssd
  ]) ++ [
    ../../environment/bluetooth.nix
    ../../environment/common.nix
    ../../environment/desktop.nix
    ../../environment/gaming.nix
    ../../service/dnscrypt.nix
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
    disk.main = {
      device = "/dev/disk/by-id/nvme-nvme.1987-3231323337393038303030313330333734364145-436f7273616972204d5036303020434f5245-00000001";
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

    nodev = {
      "/tmp" = {
        fsType = "tmpfs";
      };
    };
  };

  # Sway output configuration
  environment.etc."sway/config.d/output.conf".text = ''
    output "LG Electronics LG HDR 4K 0x0000B721" scale 2 pos 0,0
  '';

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
}

{ inputs, pkgs, ... }:

{
  imports =
    [
      ../../environments/bluetooth.nix
      ../../../services/ollama.nix
    ]
    ++ (with inputs.nixos-hardware.nixosModules; [
      common-cpu-amd
      common-gpu-amd
      common-pc-ssd
    ]);

  hardware = {
    enableRedistributableFirmware = false;
    cpu.amd.updateMicrocode = true;
    wirelessRegulatoryDatabase = true;
    firmware = with pkgs; [
      # GPU firmware:
      # amdgpu/dcn_3_2_0_dmcub.bin
      # amdgpu/gc_11_0_0_mes.bin
      # amdgpu/gc_11_0_0_mes_2.bin
      # amdgpu/gc_11_0_0_pfp.bin
      # amdgpu/polaris10_sdma.bin
      # amdgpu/psp_13_0_0_sos.bin
      # amdgpu/sdma_6_0_0.bin
      # amdgpu/smu_13_0_0.bin
      # amdgpu/vcn_4_0_0.bin
      #
      # Bluetooth firmware:
      # rtl_bt/rtl8761bu_fw.bin
      linux-firmware
    ];
  };

  boot = {
    # Use the systemd-boot EFI boot loader
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      timeout = 0;
    };

    kernelPackages = pkgs.linuxPackages_latest;
  };

  disko.devices = {
    disk = {
      main = {
        device = "/dev/disk/by-id/nvme-Corsair_MP600_CORE_212379080001303746AE";
        content = {
          type = "gpt";
          partitions = {
            boot = {
              type = "EF00";
              label = "boot";
              start = "1M";
              end = "512M";
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

      media-ssd = {
        device = "/dev/disk/by-id/nvme-CT1000P1SSD8_1911E1F0AAC7";
        content = {
          type = "gpt";
          partitions = {
            media = {
              label = "media";
              size = "100%";
              content = {
                type = "btrfs";
                extraArgs = [
                  "-L"
                  "media"
                ];
                mountpoint = "/srv/media-ssd";
                mountOptions = [
                  "noatime"
                  "nofail"
                ];
              };
            };
          };
        };
      };

      media-hdd = {
        device = "/dev/disk/by-id/usb-WD_Game_Drive_57585132453630385A544137-0:0";
        content = {
          type = "gpt";
          partitions = {
            WD_BLACK = {
              label = "WD_BLACK";
              size = "100%";
              content = {
                type = "btrfs";
                mountpoint = "/srv/media-hdd";
                mountOptions = [
                  "noatime"
                  "nofail"
                ];
              };
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
    "/var/lib/qBittorrent"
  ];

  # Sway I/O configuration
  environment.etc."sway/config.d/io.conf".text = ''
    output "HAT Kamvas Pro 16 " scale 1.25 pos 0 553
    output "LG Electronics LG HDR 4K 0x0005B621" scale 2 pos 1536 0

    input "9580:109:HID_256c:006d_Pen" {
        map_to_output "HAT Kamvas Pro 16 "
    }
  '';

  # WirePlumber device configuration
  services.pipewire.wireplumber.extraConfig = {
    alsa = {
      "monitor.alsa.rules" = [
        {
          matches = [ { "device.name" = "alsa_card.usb-Generic_USB_Condenser_Microphone_201701110001-00"; } ];
          actions = {
            update-props = {
              "device.nick" = "Microphone";
              "device.description" = "Microphone";
            };
          };
        }
        {
          matches = [
            { "node.name" = "alsa_input.usb-Generic_USB_Condenser_Microphone_201701110001-00.analog-stereo"; }
          ];
          actions = {
            update-props = {
              "node.nick" = "Microphone";
              "node.description" = "Microphone";
            };
          };
        }
        {
          matches = [ { "device.name" = "alsa_card.pci-0000_0c_00.1"; } ];
          actions = {
            update-props = {
              "device.nick" = "Monitor";
              "device.description" = "Monitor";
            };
          };
        }
        {
          matches = [ { "node.name" = "alsa_card.pci-0000_0c_00.1"; } ];
          actions = {
            update-props = {
              "device.nick" = "Monitor";
              "device.description" = "Monitor";
            };
          };
        }
        {
          matches = [
            {
              # -- RX 7900 XTX HDMI output
              "device.name" = "alsa_card.pci-0000_0b_00.1";
            }
            {
              # -- Analog audio jacks
              "device.name" = "alsa_card.pci-0000_0e_00.3";
            }
          ];

          actions = {
            update-props = {
              "device.disabled" = true;
            };
          };
        }
      ];
    };
  };

  # Configure GPU optimisations for gamemode
  programs.gamemode.settings.gpu = {
    apply_gpu_optimisations = "accept-responsibility";
    gpu_device = 2;
    amd_performance_level = "high";
  };

  services.ollama.environmentVariables = {
    ROCR_VISIBLE_DEVICES = "0";
    HSA_OVERRIDE_GFX_VERSION = "11.0.0";
  };
}

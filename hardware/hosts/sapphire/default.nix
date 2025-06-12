{
  inputs,
  config,
  pkgs,
  ...
}:

{
  imports = [
    ../../environments/laptop.nix
  ] ++ (with inputs.nixos-hardware.nixosModules; [ framework-13-7040-amd ]);

  hardware = {
    enableRedistributableFirmware = false;
    cpu.amd.updateMicrocode = true;
    firmware = with pkgs; [
      # GPU firmware:
      # amdgpu/dcn_3_1_4_dmcub.bin
      # amdgpu/gc_11_0_1_mes_2.bin
      # amdgpu/gc_11_0_1_pfp.bin
      # amdgpu/psp_13_0_4_toc.bin
      # amdgpu/sdma_6_0_1.bin
      # amdgpu/vcn_4_0_2.bin
      # amdnpu/1502_00/npu.sbin
      #
      # Bluetooth firmware:
      # mediatek/BT_RAM_CODE_MT7922_1_1_hdr.bin
      #
      # Wifi firmware:
      # mediatek/WIFI_MT7922_patch_mcu_1_1_hdr.bin
      # mediatek/WIFI_RAM_CODE_MT7922_1.bin
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
    resumeDevice = config.fileSystems."/swap".device or "";
    kernelParams = [ "resume_offset=10042260" ]; # sudo btrfs inspect-internal map-swapfile /swap/swapfile -r
    kernel.sysctl = {
      "vm.swappiness" = 1;
    };
  };

  disko.devices = {
    disk.main = {
      device = "/dev/disk/by-id/nvme-WD_BLACK_SN850X_1000GB_25134N801287";
      content = {
        type = "gpt";
        partitions = {
          boot = {
            type = "EF00";
            label = "boot";
            size = "512M";
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
              type = "luks";
              name = "crypted";
              settings = {
                allowDiscards = true;
              };
              content = {
                type = "btrfs";
                extraArgs = [
                  "-L"
                  "nixos"
                ];
                subvolumes = {
                  "/persist" = {
                    mountpoint = "/persist";
                    mountOptions = [
                      "compress=zstd"
                      "noatime"
                    ];
                  };
                  "/nix" = {
                    mountpoint = "/nix";
                    mountOptions = [
                      "compress=zstd"
                      "noatime"
                    ];
                  };
                  "/home" = {
                    mountpoint = "/home";
                    mountOptions = [
                      "compress=zstd"
                      "noatime"
                    ];
                  };
                  # Hibernation swapfile
                  "/swap" = {
                    mountpoint = "/swap";
                    swap.swapfile.size = "31398M";
                  };
                };
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

  # Compress hibernation image as much as possible
  systemd.tmpfiles.rules = [ "w /sys/power/image_size - - - - 0" ];

  # Sway I/O configuration
  environment.etc."sway/config.d/io.conf".text = ''
    output "BOE 0x0BCA Unknown" scale 1.5 pos 208 1080
    output "LG Electronics LG HDR 4K 0x0005B621" scale 2 pos 1712 1002
    output "Technical Concepts Ltd 65S535CA Unknown" scale 2 pos 0 0
  '';

  programs.captive-browser.interface = "wlan0";

  # WirePlumber device configuration
  services.pipewire.wireplumber.extraConfig = {
    alsa = {
      "monitor.alsa.rules" = [
        {
          matches = [ { "device.name" = "alsa_card.pci-0000_c1_00.1"; } ];
          actions = {
            update-props = {
              "device.nick" = "HDMI";
              "device.description" = "HDMI";
            };
          };
        }
        {
          matches = [ { "device.name" = "alsa_card.pci-0000_c1_00.6"; } ];
          actions = {
            update-props = {
              "device.nick" = "System";
              "device.description" = "System";
            };
          };
        }
        {
          matches = [ { "node.name" = "alsa_output.pci-0000_c1_00.6.analog-stereo"; } ];
          actions = {
            update-props = {
              "node.nick" = "Speaker";
              "node.description" = "Speaker";
            };
          };
        }
        {
          matches = [ { "node.name" = "alsa_input.pci-0000_c1_00.6.analog-stereo"; } ];
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

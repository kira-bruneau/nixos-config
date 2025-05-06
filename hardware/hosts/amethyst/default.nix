{ inputs, ... }:

{
  imports = (with inputs.nixos-hardware.nixosModules; [ dell-xps-13-9343 ]);

  hardware = {
    enableRedistributableFirmware = false;
    cpu.intel.updateMicrocode = true;
    # firmware = with pkgs; [
    #   # Bluetooth firmware:
    #   # brcm/BCM-0a5c-216f.hcd
    #   linux-firmware
    # ];
  };

  boot = {
    # Use the systemd-boot EFI boot loader
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      timeout = 0;
    };
  };

  disko.devices = {
    disk.main = {
      device = "/dev/disk/by-id/wwn-0x5002538844584d30";
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

  swapDevices = [
    {
      device = "/persist/swapfile";
      size = 7852;
    }
  ];

  services.logind.lidSwitch = "ignore";

  programs.captive-browser.interface = "eth0";
}

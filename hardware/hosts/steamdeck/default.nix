{ lib, pkgs, ... }:

{
  imports = [
    ../../drivers/logitech-wireless.nix
    ../../environments/gaming.nix
    ../../environments/laptop.nix
  ];

  hardware = {
    enableRedistributableFirmware = lib.mkForce false;
    cpu.amd.updateMicrocode = true;
    firmware = with pkgs; [
      # Firmware is required in stage-1 for early KMS.
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
  };

  disko.devices = {
    disk.main = {
      device = "/dev/disk/by-id/nvme-Phison_ESMP512GMB47C3-E13TS_22272M51254792";
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

  environment.persistence."/persist" = {
    directories = [
      "/home"
      "/nix"
      "/var/lib/decky-loader"
      "/var/lib/jupiter-biosupdate"
    ];

    files = [ "/var/log/jupiter-fan-control.log" ];
  };

  jovian.devices.steamdeck = {
    enable = true;
    autoUpdate = true;
  };

  environment.systemPackages = with pkgs; [ steamdeck-firmware ];

  systemd.user.services.cec-sync.environment.WAYLAND_DISPLAY = "gamescope-0";

  # Wakehook is redundant when using cec-sync
  systemd.user.services.wakehook.enable = false;

  programs.captive-browser.interface = "wlo1";
}

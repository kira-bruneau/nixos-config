{ pkgs, ... }:

{
  imports = [
    ./generated.nix
    ../../environments/laptop.nix
  ];

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
              extraArgs = [ "-F" "32" "-n" "boot" ];
              mountpoint = "/boot";
              mountOptions = [ "noatime" ];
            };
          };
          nixos = {
            label = "nixos";
            size = "100%";
            content = {
              type = "btrfs";
              extraArgs = [ "-L" "nixos" ];
              mountpoint = "/";
              mountOptions = [ "noatime" ];
            };
          };
        };
      };
    };

    nodev = {
      "/tmp" = {
        fsType = "tmpfs";
      };
    };
  };

  jovian.devices.steamdeck = {
    enable = true;
    autoUpdate = true;
  };

  environment.systemPackages = with pkgs; [
    steamdeck-firmware
  ];
}
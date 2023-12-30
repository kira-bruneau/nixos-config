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
        type = "table";
        format = "gpt";
        partitions = [
          {
            name = "boot";
            start = "0%";
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
              type = "btrfs";
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

  jovian.devices.steamdeck = {
    enable = true;
    autoUpdate = true;
  };

  environment.systemPackages = with pkgs; [
    steamdeck-firmware
  ];
}

{ inputs, pkgs, ... }:

{
  imports = (with inputs.nixos-hardware.nixosModules; [
    framework
  ]) ++ [
    ../environment/distributed-nix.nix
    ../environment/hidpi.nix
    ../environment/laptop.nix
    ../environment/locale
    ../environment/nix-ssh.nix
    ../service/dnscrypt.nix
    ../service/ssh.nix
    ../user/kira.nix
  ];

  system.stateVersion = "22.11";

  nixpkgs.hostPlatform = "x86_64-linux";

  hardware.enableRedistributableFirmware = true;

  boot = {
    # Use the systemd-boot EFI boot loader
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    kernelPackages = pkgs.linuxPackages_latest;
    kernel.sysctl = { "vm.swappiness" = 1; };
    initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
    kernelModules = [ "kvm-intel" ];
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

  # Used for hibernation (eg. when upower detects a critical battery percent)
  swapDevices = [
    {
      device = "/swap";
      size = 31898;
    }
  ];

  # Sway output configuration
  environment.etc."sway/config.d/output.conf".text = ''
    output "BOE 0x095F Unknown" scale 1.5 pos 0 77
    output "LG Electronics LG HDR 4K 0x0000B721" scale 2 pos 1504 0
  '';

  environment.etc."wpa_supplicant.conf".source = pkgs.runCommandLocal "wpa_supplicant.conf" {} ''
    ln -s /home/kira/Auth/wpa_supplicant.conf "$out"
  '';

  # Sleep on low power
  services.upower = {
    noPollBatteries = true;
    usePercentageForPolicy = false;
    timeAction = 240;
  };

  # Manage firmware updates
  services.fwupd.enable = true;
}

{ inputs, pkgs, ... }:

{
  imports = (with inputs.nixos-hardware.nixosModules; [
    common-cpu-amd
    common-gpu-amd
    common-pc-ssd
  ]) ++ [
    ../environment/desktop.nix
    ../environment/distributed-nix.nix
    ../environment/gaming.nix
    ../environment/hidpi.nix
    ../environment/locale
    ../environment/nix-ssh.nix
    ../environment/bluetooth.nix
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
    initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];

    # KVM Virtualisation
    kernelModules = [ "kvm-amd" ];
    kernelParams = [ "amd_iommu=on" ];
    binfmt.emulatedSystems = [ "armv7l-linux" "aarch64-linux" ];
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

  environment.etc."wpa_supplicant.conf".source = pkgs.runCommandLocal "wpa_supplicant.conf" {} ''
    ln -s /home/kira/Auth/wpa_supplicant.conf "$out"
  '';

  # Android debugging
  programs.adb.enable = true;

  # Configure GPU optimisations for gamemode
  programs.gamemode.settings.gpu = {
    apply_gpu_optimisations = "accept-responsibility";
    gpu_device = 0;
    amd_performance_level = "high";
  };
}

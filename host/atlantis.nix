{ config, pkgs, ... }:

{
  imports = [
    ../environment/desktop.nix
    ../environment/gaming.nix
    ../environment/hidpi.nix
    ../environment/home.nix
    ../environment/ssh-server.nix
    ../environment/wireless.nix
    ../user/builder.nix
    ../user/kira.nix
  ];

  hardware.enableRedistributableFirmware = true;

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
    kernelPackages = pkgs.linuxPackages_latest;

    # Use the systemd-boot EFI boot loader
    loader.systemd-boot.enable = true;

    # KVM Virtualisation
    kernelModules = [ "kvm-amd" ];
    kernelParams = [ "amd_iommu=on" ];
    binfmt.emulatedSystems = [ "armv7l-linux" "aarch64-linux" ];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/ad0f2168-1be3-4236-8f24-cf9e408ef611";
    fsType = "btrfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/D986-7692";
    fsType = "vfat";
  };

  nix = {
    package = pkgs.nixUnstable;
    maxJobs = 12;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  networking = {
    hostName = "atlantis";
    firewall.enable = false;
    wireless.interfaces = [
      "wlp6s0"
    ];
  };

  # Android debugging
  programs.adb.enable = true;

  # Automatically hard-link duplicate files in /nix/store
  nix.autoOptimiseStore = true;

  # Configure GPU optimisations for gamemode
  programs.gamemode.settings.gpu = {
    apply_gpu_optimisations = "accept-responsibility";
    gpu_device = 0;
    amd_performance_level = "high";
  };

  system.stateVersion = "19.03";
}

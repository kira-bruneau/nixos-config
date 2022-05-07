{ config, pkgs, ... }:

{
  imports = [
    ../environment/config.nix
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
    device = "/dev/disk/by-uuid/0d378b34-55a4-425c-ad10-3ed140d3cf54";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/47AE-E9C6";
    fsType = "vfat";
  };

  services.fstrim.enable = true;

  nix.settings = {
    auto-optimise-store = true;
    max-jobs = 12;
    experimental-features = [ "nix-command" "flakes" ];
  };

  networking = {
    hostName = "atlantis";
    firewall.enable = false;
    useDHCP = false;
    interfaces.wlp6s0.useDHCP = true;
  };

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

  system.stateVersion = "21.05";
}

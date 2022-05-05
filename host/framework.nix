{ lib, config, pkgs, ... }:

{
  imports = [
    ../environment/config.nix
    ../environment/hidpi.nix
    ../environment/home.nix
    ../environment/laptop.nix
    ../environment/ssh-server.nix
    ../user/kira.nix
  ];

  hardware = {
    enableRedistributableFirmware = true;
    cpu.intel.updateMicrocode = true;
  };

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [ "kvm-intel" ];

    # Use the systemd-boot EFI boot loader
    loader.systemd-boot.enable = true;
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/ae7476d3-f5b1-4bc7-9be8-eecf330212ba";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/6C1A-F6EA";
    fsType = "vfat";
  };

  networking = {
    hostName = "framework";
    firewall.enable = false;
    useDHCP = false;
    interfaces.wlp170s0.useDHCP = true;
    supplicant.wlp170s0.configFile.path = "/home/kira/Auth/wpa_supplicant.conf";
  };

  nix.settings = {
    auto-optimise-store = true;
    max-jobs = 8;
    experimental-features = [ "nix-command" "flakes" ];
  };

  system.stateVersion = "21.11";
}

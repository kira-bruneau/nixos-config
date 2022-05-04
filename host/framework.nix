{ config, pkgs, ... }:

{
  imports = [
    ../environment/config.nix
    ../environment/desktop.nix
    ../environment/hidpi.nix
    ../environment/home.nix
    ../environment/ssh-server.nix
    ../environment/wireless.nix
    ../user/kira.nix
  ];

  hardware.enableRedistributableFirmware = true;

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [ "kvm-intel" ];

    # Use the systemd-boot EFI boot loader
    loader.systemd-boot.enable = true;
  };

  networking = {
    hostName = "framework";
    firewall.enable = false;
    wireless.interfaces = [ "wlp170s0" ];
    supplicant."wlp170s0".configFile.path = "/home/kira/Auth/wpa_supplicant.conf";
  };

  nix.settings = {
    auto-optimise-store = true;
    max-jobs = 8;
    experimental-features = [ "nix-command" "flakes" ];
  };

  system.stateVersion = "21.11";
}

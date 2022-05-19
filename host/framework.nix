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
    opengl.extraPackages = with pkgs; [ vaapiIntel ];
  };

  boot = {
    # Use the systemd-boot EFI boot loader
    loader.systemd-boot.enable = true;

    kernelPackages = pkgs.linuxPackages_latest;
    kernel.sysctl = { "vm.swappiness" = 1; };
    initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
    kernelModules = [ "kvm-intel" ];
    kernelParams = [ "i915.enable_psr=0" ];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/ae7476d3-f5b1-4bc7-9be8-eecf330212ba";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/6C1A-F6EA";
    fsType = "vfat";
  };

  # Used for hibernation (eg. when upower detects a critical battery percent)
  swapDevices = [
    {
      device = "/swap";
      size = 31898;
    }
  ];

  networking = {
    hostName = "framework";
    firewall.enable = false;
    useDHCP = false;
    interfaces.wlp170s0.useDHCP = true;
  };

  environment.etc."wpa_supplicant.conf".source = pkgs.runCommandLocal "wpa_supplicant.conf" {} ''
    ln -s /home/kira/Auth/wpa_supplicant.conf "$out"
  '';

  nix.settings = {
    auto-optimise-store = true;
    max-jobs = 8;
    experimental-features = [ "nix-command" "flakes" ];
  };

  system.stateVersion = "21.11";
}

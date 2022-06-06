{ config, pkgs, ... }:

{
  imports = [
    ../environment/config.nix
    ../environment/hidpi.nix
    ../environment/home.nix
    ../environment/laptop.nix
    ../environment/ssh-server.nix
    ../user/kira.nix
  ];

  hardware.enableRedistributableFirmware = true;

  boot = {
    # Use the systemd-boot EFI boot loader
    loader.systemd-boot.enable = true;

    kernelPackages = pkgs.linuxPackages_latest;
    kernel.sysctl = { "vm.swappiness" = 1; };
    initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
    kernelModules = [ "kvm-intel" ];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/ae7476d3-f5b1-4bc7-9be8-eecf330212ba";
    fsType = "ext4";
    options = [ "noatime" "nodiratime" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/6C1A-F6EA";
    fsType = "vfat";
    options = [ "noatime" "nodiratime" ];
  };

  # Used for hibernation (eg. when upower detects a critical battery percent)
  swapDevices = [
    {
      device = "/swap";
      size = 31898;
    }
  ];

  nix.settings = {
    auto-optimise-store = true;
    max-jobs = 8;
    experimental-features = [ "nix-command" "flakes" ];
  };

  # Sway output configuration
  environment.etc."sway/config.d/output.conf".text = ''
    output "Unknown HP Z27k G3 CN41223C6P" scale 2 pos 0 0
    output "Unknown 0x095F 0x00000000" scale 1.5 pos 208,1080
    output "Unknown HP Z27k G3 CN41223C6V" scale 2 transform 270 pos 1920,0
    output "Goldstar Company Ltd LG HDR 4K 0x0000B721" scale 2 pos 1712,1003
  '';

  networking = {
    hostName = "framework";
    firewall.enable = false;
    useDHCP = false;
    interfaces.wlp170s0.useDHCP = true;
  };

  environment.etc."wpa_supplicant.conf".source = pkgs.runCommandLocal "wpa_supplicant.conf" {} ''
    ln -s /home/kira/Auth/wpa_supplicant.conf "$out"
  '';

  # Required by arctype to manage passwords
  services.gnome.gnome-keyring.enable = true;

  system.stateVersion = "21.11";
}

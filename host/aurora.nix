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
    ../service/ssh.nix
    ../service/syncthing.nix
    ../user/kira.nix
  ];

  system.stateVersion = "22.05";

  nixpkgs.hostPlatform = "x86_64-linux";

  hardware.enableRedistributableFirmware = true;

  boot = {
    # Use the systemd-boot EFI boot loader
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    kernelPackages = pkgs.linuxPackages;
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

  # Sway output configuration
  environment.etc."sway/config.d/output.conf".text = ''
    output "HP Inc. HP Z27k G3 CN41223C6P" scale 2 pos 0 400
    output "BOE 0x095F Unknown" scale 1.5 pos 208 1480
    output "HP Inc. HP Z27k G3 CN41223C6V" scale 2 transform 270 pos 1920 0
    output "LG Electronics LG HDR 4K 0x0000B721" scale 2 pos 1712 1403
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

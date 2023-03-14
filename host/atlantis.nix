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
    ../service/syncthing.nix
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

    tmpOnTmpfs = true;
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/0d378b34-55a4-425c-ad10-3ed140d3cf54";
    fsType = "ext4";
    options = [ "noatime" "nodiratime" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/47AE-E9C6";
    fsType = "vfat";
    options = [ "noatime" "nodiratime" ];
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

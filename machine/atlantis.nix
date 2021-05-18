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

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    # KVM Virtualisation
    kernelModules = [ "kvm-amd" ];
    kernelParams = [ "amd_iommu=on" ];
    binfmt.emulatedSystems = [ "armv7l-linux" "aarch64-linux" ];
  };

  networking = {
    hostName = "atlantis";
    firewall.enable = false;
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
}

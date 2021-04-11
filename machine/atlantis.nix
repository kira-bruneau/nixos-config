{ config, pkgs, ... }:

{
  imports = [
    ../environment/desktop.nix
    ../environment/gaming.nix
    ../environment/hidpi.nix
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

  # Locale & Timezone
  i18n.defaultLocale = "en_CA.UTF-8";
  time.timeZone = "Canada/Eastern";
  services.localtime.enable = true;

  # Enable the OpenSSH daemon
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    permitRootLogin = "no";
  };

  # Android debugging
  programs.adb.enable = true;
}

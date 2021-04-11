{ config, pkgs, ... }:

{
  imports = [
    ../environment/nur.nix
    ../environment/cli.nix
    ../environment/gui.nix
    ../environment/hidpi.nix
    ../environment/colemak.nix
    ../environment/gaming.nix
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

  networking.hostName = "atlantis";
  networking.firewall.enable = false;

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

  # Enable pipewire (sound & video)
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    media-session.enable = true;

    alsa.enable = true;
    jack.enable = true;
    pulse.enable = true;
  };

  # Enable docker
  virtualisation.docker.enable = true;

  # Disable power button
  services.logind.extraConfig = "HandlePowerKey=ignore";

  # Android debugging
  programs.adb.enable = true;

  # Power management
  services.upower.enable = true;

  # Enable flatpak
  services.flatpak.enable = true;
  xdg.portal.enable = true;
}

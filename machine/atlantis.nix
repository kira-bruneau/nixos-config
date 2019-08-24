{ config, pkgs, ... }:

{
  imports = [
    ../environment/cli.nix
    ../environment/gui.nix
    ../environment/hidpi.nix
    ../environment/colemak.nix
    ../environment/gaming.nix
    ../environment/rock64-media-client.nix
    ../user/kira.nix
  ];

  networking.hostName = "atlantis";
  networking.firewall.enable = false;
  networking.wireless.enable = true;

  # Locale & Timezone
  i18n.defaultLocale = "en_CA.UTF-8";
  time.timeZone = "Canada/Eastern";
  services.localtime.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Use amdgpu driver
  services.xserver.videoDrivers = [ "amdgpu" ];

  # Enable docker
  virtualisation.docker.enable = true;
}

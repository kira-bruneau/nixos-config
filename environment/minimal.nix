{ config, pkgs, ... }:

{
  imports = [
    ../feature/text-editor.nix
  ];

  environment.systemPackages = with pkgs; [
    curl
    htop
    ripgrep
    stow
    wget
  ];

  # Select internationalisation properties.
  i18n = {
    # consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "/etc/nixos/colemak.map";
    defaultLocale = "en_CA.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Canada/Eastern";

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Networking
  networking.hostName = "nixos";
  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
}

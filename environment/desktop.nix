{ config, pkgs, ... }:

{
  imports = [
    ./cli.nix
    ./gui.nix
  ];

  # Enable pipewire (sound & video)
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    media-session.enable = true;

    alsa.enable = true;
    jack.enable = true;
    pulse.enable = true;
  };

  # Let the desktop environment handle the power key
  services.logind.extraConfig = "HandlePowerKey=ignore";
}

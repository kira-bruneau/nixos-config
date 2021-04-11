{ config, pkgs, ... }:

{
  # Enable wifi support
  networking.wireless.enable = true;

  # Enable bluetooth support
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
  services.bluetooth-autoconnect.enable = true;

  # Enable bluetooth MPRIS proxy
  systemd.user.services.mpris-proxy = {
    description = "MPRIS proxy";
    wantedBy = [ "default.target" ];
    after = [ "network.target" "sound.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.bluez}/bin/mpris-proxy";
    };
  };
}

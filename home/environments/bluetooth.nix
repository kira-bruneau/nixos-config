{ config, ... }:

{
  services.blueman-applet.enable = config.programs ? niri;

  # Forward bluetooth media controls to MPRIS
  services.mpris-proxy.enable = true;
}

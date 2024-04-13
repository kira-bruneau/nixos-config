{ config, ... }:

{
  services.blueman-applet.enable = config.wayland.windowManager.sway.enable;

  # Forward bluetooth media controls to MPRIS
  services.mpris-proxy.enable = true;
}

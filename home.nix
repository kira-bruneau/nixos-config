{ config, pkgs, ... }:

{
  # TODO: Generate configuration from Nix
  services.polybar = {
    enable = true;
    package = pkgs.polybarFull;
    script = ''
      export WLAN_INTERFACE=$(ip link show | grep wlp | awk -F':' '{print $2}' | awk '{print $1}')
      polybar --reload main
    '';
  };
}

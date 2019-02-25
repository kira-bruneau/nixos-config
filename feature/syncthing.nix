{ config, pkgs, ... }:

{
  services.syncthing = {
    enable = true;
    systemService = false;
  };
}

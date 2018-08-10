{ config, pkgs, ... }:

{
  fileSystems."/virtualboxshare" = {
    fsType = "vboxsf";
    device = "share";
    options = [ "rw" ];
  };
}

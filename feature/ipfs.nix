{ config, pkgs, ... }:

{
  services.ipfs = {
    enable = true;
    autoMount = true;
    emptyRepo = true;
  };
}

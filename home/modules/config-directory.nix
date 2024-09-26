{ config, lib, ... }:

{
  options = {
    home.configDirectory = lib.mkOption {
      type = lib.types.path;
      default = "${config.home.homeDirectory}/Dev/public/nixos-config/main/home";
    };
  };
}

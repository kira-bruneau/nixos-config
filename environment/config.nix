{ config, lib, ... }:

with lib;
{
  options.home.configDirectory = mkOption {
    type = types.path;
    default = "${config.home.homeDirectory}/Dev/home-config";
  };
}

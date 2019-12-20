{ config, pkgs, ... }:

{
  # Add home bin to PATH
  environment.homeBinInPath = true;

  # Packages
  environment.systemPackages = with pkgs; [
    sudo
  ];
}

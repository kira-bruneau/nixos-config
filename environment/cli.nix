{ config, pkgs, ... }:

{
  imports = [
    ./nur.nix
  ];

  # Add home bin to PATH
  environment.homeBinInPath = true;

  # Packages
  environment.systemPackages = with pkgs; [
    sudo
  ];
}

{ config, pkgs, ... }:

{
  # My emacs configuration will automatically install system packages
  # using system-packages.el.
  environment.systemPackages = with pkgs; [
    emacs
    git # required to clone system-packages.el
  ];
}

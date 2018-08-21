{ config, pkgs, ... }:

{
  # My emacs configuration will automatically install system packages
  # using system-packages.el.
  environment.systemPackages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    emacs
    git # required to clone system-packages.el
  ];
}

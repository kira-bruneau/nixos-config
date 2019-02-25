{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    curl
    fd
    fzf
    htop
    jq
    nix-index
    nmap
    ripgrep
    stow
    sudo
    tmux
    wget
    youtube-dl
  ];
}

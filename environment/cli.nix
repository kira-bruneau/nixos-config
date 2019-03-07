{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    cloc
    curl
    fd
    file
    fzf
    git
    htop
    jq
    nethogs
    nix-index
    nix-prefetch-scripts
    nmap
    python3
    ripgrep
    stow
    sudo
    tmux
    unzip
    wget
    xorg.lndir
    youtube-dl
  ];
}

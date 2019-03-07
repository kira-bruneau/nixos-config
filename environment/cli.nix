{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    cloc
    curl
    fd
    fzf
    git
    htop
    jq
    nix-index
    nmap
    python3
    ripgrep
    stow
    sudo
    tmux
    wget
    youtube-dl
  ];
}

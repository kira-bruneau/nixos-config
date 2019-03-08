{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    cloc
    curl
    fd
    file
    fzf
    gdb
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
    strace
    sudo
    tcpflow
    tmux
    unzip
    valgrind
    wget
    xorg.lndir
    youtube-dl
  ];
}

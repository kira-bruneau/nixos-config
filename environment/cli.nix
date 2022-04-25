{ pkgs, ... }:

let
  whichf = pkgs.writeShellScriptBin "whichf" ''
    readlink -f $(which $@)
  '';
in
{
  imports = [
    ../package/aspell
    ../package/bash
    ../package/direnv
    ../package/emacs
    ../package/git
    ../package/gpg
    ../package/htop
    ../package/ssh
    ../package/tmux
    ../package/fzf
  ];

  # Packages
  home.packages = with pkgs; [
    # Core utils
    bat
    du-dust
    exa
    sd
    tealdeer
    whichf

    # Administration
    evtest
    nethogs
    pciutils
    topgrade

    # Search
    fd
    ripgrep

    # Networking
    curl
    netcat
    nmap
    wget
    whois

    # Shell
    bash-completion
    nix-bash-completions

    # Data conversion & manipulation
    ffmpeg
    jq
    p7zip
    poke
    unrar
    unzip
    xmlstarlet

    # General development
    binutils
    cloc
    file
    linuxPackages.perf
    tokei

    # Nix development
    cachix
    carnix
    nix-index
    nix-prefetch-scripts
    nixpkgs-fmt
    nixpkgs-review
    nodePackages.node2nix
    patchelf

    # Debuggers
    strace
    tcpflow
    valgrind

    # Multimedia
    yabridge
    yabridgectl
    youtube-dl
  ];

  programs.man.enable = true;
}

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
    du-dust
    sd
    tealdeer
    whichf

    # Administration
    nethogs
    pciutils

    # Search
    fd
    ripgrep

    # Networking
    curl
    netcat
    nmap
    wget
    whois

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
    file
    linuxPackages.perf
    tokei

    # Nix development
    cachix
    nix-index
    nixpkgs-review
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

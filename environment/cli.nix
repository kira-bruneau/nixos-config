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
  ];

  # Packages
  home.packages = with pkgs; [
    # Administration
    evtest
    nethogs
    pciutils
    stow
    topgrade
    whichf
    xorg.lndir

    # Search
    broot
    fd
    fzf
    ripgrep

    # Networking
    curl
    netcat
    nmap
    openconnect
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
    unrar # unfreeRedistributable
    unzip
    xmlstarlet

    # General development
    binutils
    cloc
    file
    linuxPackages.perf
    man-pages

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
    gdb
    lldb
    strace
    tcpflow
    valgrind

    # Interpreters
    python3

    # Multimedia
    yabridge
    yabridgectl
    youtube-dl
  ];
}

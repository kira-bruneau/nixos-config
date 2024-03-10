{ pkgs, ... }:

{
  imports = [
    ../programs/direnv
    ../programs/emacs
    ../programs/git
  ];

  home.packages = with pkgs; [
    # Media & Documents
    poke
    sqlitebrowser

    # Nix development
    cachix
    nix-bisect
    nix-index
    nix-init
    nix-output-monitor
    nixpkgs-review
    nurl
    patchelf

    # General development
    binutils
    file
    linuxPackages.perf
    tokei

    # Debuggers
    strace
    tcpflow
    valgrind
  ];
}

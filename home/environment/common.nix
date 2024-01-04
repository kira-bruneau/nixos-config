{ pkgs, inputs, ... }:

let
  whichf = pkgs.writeShellScriptBin "whichf" ''
    readlink -f $(which $@)
  '';
in
{
  nixpkgs.overlays = [
    inputs.kira-nur.overlays.default
  ];

  imports = [
    ../package/bash
    ../package/direnv
    ../package/emacs
    ../package/fzf
    ../package/git
    ../package/gpg
    ../package/htop
    ../package/ssh
    ../package/tmux
    ./seed-home-config.nix
  ];

  home.packages = with pkgs; [
    # Administration
    nethogs
    pciutils

    # Networking
    curl
    netcat
    nmap
    whois

    # Data conversion & manipulation
    jq
    p7zip
    unrar
    unzip
    xmlstarlet

    # Coreutils alternatives
    du-dust
    fd
    ripgrep
    sd

    # Custom utils
    whichf
  ];

  programs.man.enable = true;
}

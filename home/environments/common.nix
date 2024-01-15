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
    ../programs/bash
    ../programs/direnv
    ../programs/emacs
    ../programs/fzf
    ../programs/git
    ../programs/gpg
    ../programs/htop
    ../programs/ssh
    ../programs/tmux
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

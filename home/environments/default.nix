{ pkgs, ... }:

let
  whichf = pkgs.writeShellScriptBin "whichf" ''
    readlink -f $(which $@)
  '';
in
{
  imports = [
    ../programs/bash
    ../programs/fzf
    ../programs/gpg
    ../programs/htop
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

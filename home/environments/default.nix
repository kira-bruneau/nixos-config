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
    ../programs/git
    ../programs/htop
    ../programs/ssh
    ../programs/tmux
  ];

  home.packages = with pkgs; [
    # Networking
    curl
    dig
    netcat
    nmap
    rsync
    whois

    # Data conversion & manipulation
    jq
    p7zip
    unrar
    unzip
    xmlstarlet
    zip

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

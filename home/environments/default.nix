{ lib, pkgs, ... }:

let
  whichf = pkgs.writeShellScriptBin "whichf" ''
    readlink -f $(which $@)
  '';
in
{
  imports = [
    ../modules
    ../programs/bash
    ../programs/fish
    ../programs/fzf
    ../programs/git
    ../programs/htop
    ../programs/ssh
    ../programs/tmux
    ./nix-gc.nix
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
    ffmpeg
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

  dconf.enable = lib.mkDefault false;
}

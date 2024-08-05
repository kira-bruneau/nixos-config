{ pkgs, pkgsNixIndexDatabase, ... }:

let
  whichf = pkgs.writeShellScriptBin "whichf" ''
    readlink -f $(which $@)
  '';
in
{
  imports = [
    ../modules
    ../programs/bash
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

    # Nix utils
    pkgsNixIndexDatabase.comma-with-db

    # Custom utils
    whichf
  ];

  programs.man.enable = true;

  programs.nix-index = {
    enable = true;
    package = pkgsNixIndexDatabase.nix-index-with-db;
  };
}

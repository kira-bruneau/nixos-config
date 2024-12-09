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

  systemd.user.services.taildrop = {
    Install.WantedBy = [ "default.target" ];
    Unit.Description = "Automatically save taildrop files to ~/Downloads/Taildrop";
    Service = {
      ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p '%h/Downloads/Taildrop'";
      ExecStart = "${lib.getExe pkgs.tailscale} file get --loop '%h/Downloads/Taildrop'";
    };
  };
}

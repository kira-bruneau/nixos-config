{
  lib,
  pkgs,
  pkgsKiraNur,
  ...
}:

let
  jb = pkgs.writeShellApplication {
    name = "jb";

    runtimeInputs = with pkgs; [ systemd ];

    text = ''
      if [ $# -gt 0 ]; then
        journalctl -b -u "$@"
      else
        journalctl -b
      fi
    '';
  };

  ji = pkgs.writeShellApplication {
    name = "ji";

    runtimeInputs = with pkgs; [ systemd ];

    text = ''
      journalctl -I -u "$@"
    '';
  };

  sudir = pkgs.writeShellApplication {
    name = "sudir";
    text = ''
      exec sudo -u "$(stat -c "%U" "$1")" sh -c 'cd $0 && exec env HOME=/var/empty $1' "$1" "$SHELL"
    '';
  };

  wf = pkgs.writeShellApplication {
    name = "wf";

    runtimeInputs = with pkgs; [
      coreutils
      which
    ];

    text = ''
      readlink -f "$(which "$@")"
    '';
  };
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
    jq
    p7zip
    unrar
    unzip
    xmlstarlet
    zip

    # Coreutils alternatives
    dust
    pkgsKiraNur.fd-relative-full-path
    ripgrep
    sd

    # Custom utils
    jb
    ji
    sudir
    wf
  ];

  # home-manager enables dconf by default, but we only want it in the gui environments
  dconf.enable = lib.mkDefault false;
}

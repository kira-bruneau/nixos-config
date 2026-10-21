{
  lib,
  pkgs,
  pkgsKiraNur,
  pkgsCustom,
  ...
}:

{
  imports = [
    ../modules
    ../programs/bash
    ../programs/fish
    ../programs/nushell
    ../programs/skim
    ../programs/git
    ../programs/htop
    ../programs/ssh
    ../programs/tmux
    ./nix-gc.nix
  ];

  home.packages =
    (with pkgs; [
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
    ])
    ++ (with pkgsCustom; [
      # Custom utils
      jb
      ji
      sudir
      wf
    ]);

  # home-manager enables dconf by default, but we only want it in the gui environments
  dconf.enable = lib.mkDefault false;
}

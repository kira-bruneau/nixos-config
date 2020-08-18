{ config, pkgs, ... }:

{
  imports = [
    ../../../../emacs/.emacs.d/home.nix
  ];

  # Packages
  home.packages = with pkgs; [
    # Administration
    evtest
    htop
    nethogs
    pciutils
    stow
    tmux
    topgrade
    xorg.lndir

    # Authentication
    gnupg

    # Search
    broot
    fd
    fzf
    ripgrep

    # Networking
    curl
    netcat
    nmap
    openconnect
    wget
    whois

    # Shell
    bash-completion
    nix-bash-completions

    # Data conversion & manipulation
    ffmpeg
    jq
    p7zip
    unrar
    unzip
    xmlstarlet

    # General development
    binutils
    cloc
    file
    git # TODO: Generate configuration from Nix
    man-pages

    # Nix development
    cachix
    carnix
    nix-index
    nix-prefetch-scripts
    nixpkgs-fmt
    nodePackages.node2nix
    patchelf

    # Debuggers
    gdb
    lldb
    strace
    tcpflow
    valgrind

    # Interpreters
    python3

    # Misc
    youtube-dl
  ];

  # Add home bin to PATH
  # environment.homeBinInPath = true;

  home.sessionVariables = {
    EDITOR = "emacseditor";
  };

  # Bash
  programs.bash = {
    enable = true;

    historySize = -1;
    historyFileSize = -1;
    historyControl = [ "ignoredups" ];

    initExtra = ''
      trap 'history -a' DEBUG            # Immediately append commands to history
      export PROMPT_COMMAND='history -n' # Read unread history at every prompt
      stty -ixon                         # Fix forward history searching
    '';

    shellAliases = {
      # Convenience aliases for common nixos commands
      nbuild = "sudo nixos-rebuild switch && home-manager switch";
      nup = "sudo nixos-rebuild --upgrade switch && nix-channel --update && home-manager switch";
      ntest = "sudo nixos-rebuild test && home-manager switch";
      br = "broot";
    };
  };

  # Direnv
  programs.direnv.enable = true;

  # Enable gpg agent
  services.gpg-agent.enable = true;
}

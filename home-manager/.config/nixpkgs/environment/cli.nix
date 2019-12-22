{ config, pkgs, ... }:

{
  imports = [
    ../../../../emacs/.emacs.d/home.nix
  ];

  # Add home bin to PATH
  # environment.homeBinInPath = true;

  pam.sessionVariables = {
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

  # Packages
  home.packages = with pkgs; [
    # Administration utils
    evtest
    htop
    nethogs
    pciutils
    stow
    tmux
    xorg.lndir

    # Search utils
    broot
    fd
    fzf
    ripgrep

    # Networking utils
    curl
    nmap
    openconnect
    wget
    whois

    # Data conversion & manipulation utils
    ffmpeg
    jq
    p7zip
    unrar
    unzip
    xmlstarlet

    # General dev utils
    cloc
    file
    git # TODO: Generate configuration from Nix
    man-pages

    # Nix dev utils
    cachix
    carnix
    nix-index
    nix-prefetch-scripts
    nodePackages.node2nix
    patchelf

    # Build systems & compilers
    binutils
    cargo
    cargo-edit
    clang
    clang-manpages
    rustc

    # Debuggers
    lldb
    strace
    tcpflow
    valgrind

    # Interpreters
    python3

    # Misc
    youtube-dl
  ];
}

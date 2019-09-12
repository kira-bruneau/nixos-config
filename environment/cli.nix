{ config, pkgs, ... }:

{
  # Services
  services = {
    emacs = {
      enable = true;
      defaultEditor = true;
    };

    syncthing = {
      enable = true;
      systemService = false;
    };
  };

  # Packages
  environment.systemPackages = with pkgs; [
    # Administration utils
    evtest
    htop
    nethogs
    stow
    sudo
    tmux
    xorg.lndir

    # Search utils
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
    git
    man-pages

    # Nix dev utils
    nix-index
    nix-prefetch-scripts
    patchelf

    # Build systems & compilers
    cargo
    cargo-edit
    clang
    clang-manpages
    llvmPackages.bintools
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

  # Unlimited bash history synchronized between each terminal
  programs.bash.interactiveShellInit = ''
    export HISTSIZE=""
    export HISTFILESIZE=""
    export HISTCONTROL="ignoredups";
    trap 'history -a' DEBUG            # Immediately append commands to history
    export PROMPT_COMMAND='history -n' # Read unread history at every prompt
    shopt -s histappend                # Append instead of rewrite history on exit
    stty -ixon                         # Fix forward history searching
  '';
}

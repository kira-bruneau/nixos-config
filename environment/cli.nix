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
    cloc
    curl
    fd
    file
    fzf
    gdb
    git
    htop
    jq
    nethogs
    nix-index
    nix-prefetch-scripts
    nmap
    python3
    ripgrep
    stow
    strace
    sudo
    tcpflow
    tmux
    unzip
    valgrind
    wget
    xorg.lndir
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

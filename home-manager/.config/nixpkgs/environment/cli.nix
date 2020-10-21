{ config, pkgs, ... }:

{
  imports = [
    ../../../../emacs/.emacs.d/home.nix
  ];

  # Packages
  home.packages = with pkgs; with nur.repos.metadark; [
    # Administration
    bluetooth-autoconnect
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
    unrar # unfreeRedistributable
    unzip
    xmlstarlet

    # General development
    binutils
    cloc
    file
    git # TODO: Generate configuration from Nix
    linuxPackages.perf
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

  # LanguageTool Server
  systemd.user.services.languagetool = {
    Unit = {
      Description = "LanguageTool embedded HTTP Server";
      Documentation = [ "https://dev.languagetool.org/http-server" ];
    };
    Service = {
      ExecStart = ''
        ${pkgs.languagetool}/bin/languagetool-http-server \
          --allow-origin '*' \
          --config ${pkgs.writeText "languagetool-config" ''
            languageModel=${pkgs.linkFarm "languageModel" [
              {
                name = "en";
                path = pkgs.fetchzip {
                  url = "https://languagetool.org/download/ngram-data/ngrams-en-20150817.zip";
                  sha256 = "1hgjilpgdzbs9kgksq1jl0f6y8ff76mn6vlicc1d8zj943l2cxmz";
                  extraPostFetch = "chmod -R a-w $out";
                };
              }
            ]}
            word2vecModel=${pkgs.linkFarm "word2vecModel" [
              {
                name = "en";
                path = pkgs.fetchzip {
                  url = "https://languagetool.org/download/word2vec/en.zip";
                  sha256 = "1w5vv9b5s7mla4ywrqa54fxqrqcaw5yl1jc23pj1f75ir89p811w";
                  extraPostFetch = "chmod -R a-w $out";
                };
              }
            ]}
            fasttextModel=${pkgs.fetchurl {
              url = "https://dl.fbaipublicfiles.com/fasttext/supervised-models/lid.176.bin";
              sha256 = "0kkncb1swi2azh0ci7kq0sfg1mw559wy8jafhk3iq9mwa5afqsby";
            }}
            fasttextBinary=${pkgs.fasttext}/bin/fasttext
          ''}
      '';
      Restart = "on-failure";
    };
    Install.WantedBy = [ "default.target" ];
  };
}

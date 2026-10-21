{
  pkgs,
  pkgsNixIndexDatabase,
  pkgsCustom,
  ...
}:

{
  imports = [
    ./graphics.nix
    ../../programs/direnv
    ../../programs/emacs
    ../../programs/go
    ../../programs/nix-init
    ../../programs/zeal
  ];

  home.packages = with pkgs; [
    # Web
    ungoogled-chromium
    wget2

    # Media & Documents
    libxslt # xsltproc
    poke

    # Databases
    dbeaver-bin

    # Nix development
    cachix
    comma
    nix-bisect
    nix-output-monitor
    nix-search
    nixpkgs-review
    nurl
    patchelf
    pkgsCustom.nid

    # Android development
    android-tools

    # General development
    binutils
    file
    git-bug
    jujutsu
    perf
    tokei

    # Debuggers
    gdb
    strace
    tcpflow
    valgrind
  ];

  wayland.windowManager.sway.config = {
    assigns."1" = [ { app_id = "^chromium-browser$"; } ];
  };

  programs.nix-index = {
    enable = true;
    package = pkgsNixIndexDatabase.nix-index-with-db;
  };

  programs.nushell.plugins = with pkgs.nushellPlugins; [
    polars
  ];
}

{
  config,
  pkgs,
  pkgsNixIndexDatabase,
  ...
}:

let
  nix-instant-dev = pkgs.writeShellApplication {
    name = "nid";

    bashOptions = [ ];

    runtimeInputs = with pkgs; [
      config.nix.package
      coreutils
      git
      jq
    ];

    text = builtins.replaceStrings [ "nix-instant-dev.cmake" ] [ "${./nix-instant-dev.cmake}" ] (
      builtins.readFile ./nix-instant-dev.sh
    );
  };
in
{

  imports = [
    ../../programs/direnv
    ../../programs/emacs
    ../../programs/go
    ../../programs/nix-init
  ];

  home.packages = with pkgs; [
    # Web
    ungoogled-chromium

    # Media & Documents
    poke
    sqlitebrowser

    # Nix development
    cachix
    comma
    nix-bisect
    nix-instant-dev
    nix-output-monitor
    nix-search
    nixpkgs-review
    nurl
    patchelf

    # General development
    binutils
    difftastic
    file
    gitAndTools.git-bug
    linuxPackages.perf
    tokei

    # Debuggers
    gdb
    strace
    tcpflow
    valgrind
  ];

  programs.niri.settings = {
    window-rules = [
      {
        matches = [ { app-id = "^chromium-browser$"; } ];
        open-on-workspace = "1-browsing";
        open-maximized = true;
      }
    ];
  };

  programs.nix-index = {
    enable = true;
    package = pkgsNixIndexDatabase.nix-index-with-db;
  };
}

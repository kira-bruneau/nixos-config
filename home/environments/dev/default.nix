{
  config,
  pkgs,
  pkgsChromium,
  pkgsNixIndexDatabase,
  ...
}:

let
  nix-instant-dev = pkgs.writeShellApplication {
    name = "nid";

    bashOptions = [ ];

    excludeShellChecks = [ "SC2154" ];

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
  ];

  home.packages = with pkgs; [
    # Web
    pkgsChromium.ungoogled-chromium

    # Media & Documents
    poke
    sqlitebrowser

    # Nix development
    cachix
    pkgsNixIndexDatabase.comma-with-db
    nix-bisect
    nix-init
    nix-instant-dev
    nix-output-monitor
    nixpkgs-review
    nurl
    patchelf

    # General development
    binutils
    difftastic
    file
    linuxPackages.perf
    tokei

    # Debuggers
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
}

{
  config,
  pkgs,
  pkgsChromium,
  ...
}:

let
  nix-instant-dev = pkgs.writeShellApplication {
    name = "nid";

    bashOptions = [
      "errexit"
      "pipefail"
    ];

    runtimeInputs = with pkgs; [
      config.nix.package
      coreutils
      git
      gnused
    ];

    text = builtins.readFile ./nix-instant-dev.sh;
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
    nix-bisect
    nix-init
    nix-instant-dev
    nix-output-monitor
    nixpkgs-review
    nurl
    patchelf

    # General development
    binutils
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
}

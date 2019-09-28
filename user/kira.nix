{ config, pkgs, ... }:

{
  users.users.kira = {
    uid = 1000;
    description = "Kira Bruneau";
    extraGroups = [ "wheel" "docker" ];
    isNormalUser = true;
  };

  environment.systemPackages = with pkgs; [
    # Packages required by my emacs config.
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    ccls
    diffutils
    fd
    fzf
    gdb
    git
    jdk
    nodePackages.bash-language-server
    nodePackages.typescript-language-server
    nodejs
    pandoc
    python37Packages.pycodestyle
    python37Packages.pyflakes
    python37Packages.python-language-server
    python37Packages.rope
    python37Packages.yapf
    ripgrep
    rls
    rustc
    rustracer

    # Packages required by my i3 config.
    alacritty
    dunst
    feh
    firefox
    keepassxc
    liferea
    light
    pavucontrol
    polybarFull
    python3
    rofi
    scrot
    sound-theme-freedesktop
    xcwd
    xorg.xdpyinfo

    # Packages required by my sway config.
    alacritty
    dunst
    grim
    keepassxc
    light
    pavucontrol
    rofi
    slurp
    sound-theme-freedesktop
    (waybar.override {
      pulseSupport = true;
      mpdSupport = false;
    })
    wl-clipboard
    xdg-user-dirs
  ];

  environment.variables = {
    RUST_SRC_PATH = "${pkgs.rustPlatform.rustcSrc}";
  };

  # Fonts required by my i3 config.
  fonts.fonts = with pkgs; [
    font-awesome_5
  ];

  # Services required by my i3 config.
  services.compton = {
    enable = true;
    backend = "glx";
    shadow = true;
    shadowExclude = [
      "class_i = 'i3-frame'" # titlebars
      "name = 'Polybar tray window'" # polybar tray
      "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'" # background windows in tabbed layout
    ];
    fade = true;
    fadeDelta = 10; # 100 steps per second
    fadeSteps = [ "0.0666" "0.0444" ]; # ~150ms ~225ms
    settings = {
      clear-shadow = true;
      blur-background = true;
      no-fading-destroyed-argb = true;
    };
  };

  services.redshift.enable = true;
  location.provider = "geoclue2";

  # TODO: automatically clone dotfiles / emacs config to home
  # See: https://github.com/rycee/home-manager
}

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
    fd
    fzf
    gdb
    git
    nodejs
    pandoc
    python37Packages.autopep8
    python37Packages.flake8
    python37Packages.yapf
    ripgrep
    rustracer

    # Packages required by my i3 config.
    alacritty
    compton
    dunst
    feh
    keepassxc
    light
    pavucontrol
    (polybar.override {
      i3Support = true;
      pulseSupport = true;
    })
    redshift
    rofi
    scrot
    sound-theme-freedesktop
    xcwd
  ];

  # Fonts required by my i3 config.
  fonts.fonts = with pkgs; [
    font-awesome_5
  ];

  # Services required by my i3 config.
  services.geoclue2.enable = true;

  # TODO: automatically clone dotfiles / emacs config to home
  # See: https://github.com/rycee/home-manager
}

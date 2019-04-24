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
    # Most system packages will be automatically installed through
    # system-packages.el.
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    git # required to clone system-packages.el

    # Packages required by my i3 config.
    alacritty
    compton
    dunst
    feh
    light
    polybar
    redshift
    rofi
    scrot
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

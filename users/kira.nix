{ config, pkgs, ... }:

{
  users.users.kira = {
    uid = 1000;
    description = "Kira Bruneau";
    extraGroups = [ "wheel" "docker" ];
    isNormalUser = true;
  };

  # TODO: automatically clone dotfiles / emacs config to home
  # See: https://github.com/rycee/home-manager
}

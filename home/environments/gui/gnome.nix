{ pkgs, ... }:

{
  imports = [
    ./.

    # Media & Documents
    ../../programs/evince
    ../../programs/loupe

    # Themes
    ../../programs/gtk
  ];

  home.packages = with pkgs; [
    # Administration
    baobab
    gnome.dconf-editor
    gnome.gnome-disk-utility
    gnome.gnome-system-monitor

    # Media & Documents
    gnome.file-roller
    gnome.nautilus

    # Utils
    gnome-console
    gnome.gnome-clocks
    gnome.seahorse
  ];

  programs.bash.enableVteIntegration = true;
  programs.zsh.enableVteIntegration = true;
}

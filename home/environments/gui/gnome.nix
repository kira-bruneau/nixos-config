{
  imports = [
    ./.

    # Media & Documents
    ../../programs/evince
    ../../programs/loupe

    # Themes
    ../../programs/gtk
  ];

  home.packages = [
    gnome.gnome-clocks
  ];
}

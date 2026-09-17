{
  lib,
  pkgs,
  ...
}:

let
  settingsFormat = pkgs.formats.ini { };
in
{
  home.packages = with pkgs; [
    zeal
  ];

  xdg.configFile."Zeal/Zeal.conf".source = settingsFormat.generate "Zeal.conf" {
    General.check_for_update = false;

    content = {
      appearance = ''@Variant(\0\0\0\x7f\0\0\0\x12\x43ontentAppearance\0\0\0\0\x2)''; # dark
      fixed_font_family = "mono";
      sans_serif_font_family = "sans-serif";
      serif_font_family = "system-ui";
    };

    internal.version = "0.8.1";

    # Fullscreen & widen sidebar
    state = {
      splitter_geometry = ''@ByteArray(\0\0\0\xff\0\0\0\x1\0\0\0\x2\0\0\x1!\0\0\x5q\0\xff\xff\xff\xff\x1\0\0\0\x1\0)'';
      toc_splitter_state = "@ByteArray()";
      window_geometry = ''@ByteArray(\x1\xd9\xd0\xcb\0\x3\0\0\0\0\0\0\0\0\0\0\0\0\x6\x95\0\0\x4)\0\0\0\0\0\0\0\0\0\0\x6\x95\0\0\x4)\0\0\0\0\x2\0\0\0\x6\x9c\0\0\0\0\0\0\0\0\0\0\x6\x95\0\0\x4))'';
    };

    ui.hide_menu_bar = true;
  };

  dconf.settings = {
    "org/gnome/settings-daemon/plugins/media-keys" = {
      custom-keybindings = [
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/zeal/"
      ];
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/zeal" = {
      name = "Open Zeal";
      command = lib.getExe pkgs.zeal;
      binding = "<Super>Z";
    };
  };
}

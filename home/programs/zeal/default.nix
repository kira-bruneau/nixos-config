{
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

    ui.hide_menu_bar = true;
  };
}

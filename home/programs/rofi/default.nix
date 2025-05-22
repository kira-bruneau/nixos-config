{
  pkgs,
  ...
}:

{
  imports = [ ./themes.nix ];

  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
  };

  home.packages = with pkgs; [ rofimoji ];

  programs.niri.settings = {
    binds = {
      "Mod+A".action.spawn = [
        "rofi"
        "-show"
        "drun"
        "-theme"
        "icon-grid"
        "-matching"
        "fuzzy"
      ];

      "Mod+X".action.spawn = [
        "rofi"
        "-show"
        "run"
        "-matching"
        "fuzzy"
      ];

      "Mod+W".action.spawn = [
        "rofi"
        "-show"
        "window"
        "-matching"
        "fuzzy"
      ];

      "Mod+S".action.spawn = [
        "rofi"
        "-show"
        "ssh"
        "-matching"
        "fuzzy"
      ];

      "Mod+M".action.spawn = "rofimoji";
    };
  };
}

{
  config,
  options,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) types;

  cfg = config.programs.gnome-pomodoro;

  actions = lib.imap0 (i: action: {
    name = "org/gnome/pomodoro/plugins/actions/action${toString i}";
    value = action;
  }) cfg.actions;
in
{
  options = {
    programs.gnome-pomodoro = {
      enable = lib.mkEnableOption "gnome-pomodoro";

      package = lib.mkPackageOption pkgs [
        "gnome"
        "pomodoro"
      ] { };

      actions = lib.mkOption {
        type = types.listOf options.dconf.settings.type.nestedTypes.elemType;
        default = [ ];
      };

      settings = lib.mkOption {
        type = options.dconf.settings.type.nestedTypes.elemType;
        default = { };
      };
    };
  };

  config = {
    home.packages = [ cfg.package ];

    programs.gnome-pomodoro.settings = lib.mkIf (cfg.actions != [ ]) {
      enabled-plugins = [ "actions" ];
    };

    dconf.settings = {
      "org/gnome/pomodoro/preferences" = cfg.settings;
      "org/gnome/pomodoro/plugins/actions" = {
        actions-list = (builtins.map (action: "/${action.name}/") actions);
      };
    } // builtins.listToAttrs actions;
  };
}

{ lib, config, pkgs, ... }:

{
  home.packages = with pkgs; [
    gnome.pomodoro
  ];

  dconf.settings = lib.mkMerge [
    {
      "org/gnome/pomodoro/preferences" = {
        enabled-plugins = [
          "dark-theme"
          "sounds"
        ];
      };
    }
    (lib.mkIf config.programs.mako.enable {
      "org/gnome/pomodoro/preferences" = {
        enabled-plugins = [
          "actions"
        ];
      };

      "org/gnome/pomodoro/plugins/actions" = {
        actions-list = [
          "/org/gnome/pomodoro/plugins/actions/action0/"
          "/org/gnome/pomodoro/plugins/actions/action1/"
        ];
      };

      "org/gnome/pomodoro/plugins/actions/action0" =
        let
          timeout = toString (config.programs.mako.defaultTimeout / 1000);
        in
        {
          name = "Start Pomodoro";
          states = [ "pomodoro" ];
          triggers = [ "start" ];
          command = "sh -c 'makoctl mode -a sticky & sleep ${timeout} && makoctl mode -a invisible'";
        };

      "org/gnome/pomodoro/plugins/actions/action1" = {
        name = "End Pomodoro";
        states = [ "pomodoro" ];
        triggers = [ "complete" "skip" ];
        command = "makoctl mode -r sticky -r invisible";
      };
    })
  ];
}

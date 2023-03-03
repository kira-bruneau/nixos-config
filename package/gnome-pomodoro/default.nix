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
          "/org/gnome/pomodoro/plugins/actions/action2/"
          "/org/gnome/pomodoro/plugins/actions/action3/"
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
          command = toString (pkgs.writeShellScript "start-pomodoro.sh" ''
            set -eu -o pipefail
            swaymsg 'gaps inner all set 0' &
            swaymsg 'bar mode hide' &
            makoctl mode -a sticky &
            sleep ${timeout} && makoctl mode | grep -q sticky && makoctl mode -a invisible &
          '');
        };

      "org/gnome/pomodoro/plugins/actions/action1" = {
        name = "Pause Pomodoro";
        states = [ "pomodoro" ];
        triggers = [ "pause" ];
        command = toString (pkgs.writeShellScript "pause-pomodoro.sh" ''
          set -eu -o pipefail
          swaymsg 'bar mode dock' &
        '');
      };

      "org/gnome/pomodoro/plugins/actions/action2" = {
        name = "Resume Pomodoro";
        states = [ "pomodoro" ];
        triggers = [ "resume" ];
        command = toString (pkgs.writeShellScript "resume-pomodoro.sh" ''
          set -eu -o pipefail
          swaymsg 'bar mode hide' &
        '');
      };

      "org/gnome/pomodoro/plugins/actions/action3" = {
        name = "End Pomodoro";
        states = [ "pomodoro" ];
        triggers = [ "complete" "skip" ];
        command = toString (pkgs.writeShellScript "end-pomodoro.sh" ''
          set -eu -o pipefail
          swaymsg 'gaps inner all set 10' &
          swaymsg 'bar mode dock' &
          makoctl mode -r sticky -r invisible &
        '');
      };
    })
  ];
}

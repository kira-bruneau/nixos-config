{ lib, config, pkgs, ... }:

let
  package = pkgs.gnome.pomodoro;
  gnome-pomodoro = "${package}/bin/gnome-pomodoro";
  makoctl = "${config.services.mako.package}/bin/makoctl";
  swaymsg = "${config.wayland.windowManager.sway.package}/bin/swaymsg";
in
{
  home.packages = [ package ];

  dconf.settings = lib.mkMerge [
    {
      "org/gnome/pomodoro/preferences" = {
        enabled-plugins = [
          "dark-theme"
          "sounds"
        ];
      };
    }
    (lib.mkIf config.services.mako.enable {
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
          timeout = toString (config.services.mako.defaultTimeout / 1000);
        in
        {
          name = "Start Pomodoro";
          states = [ "pomodoro" ];
          triggers = [ "start" ];
          command = toString (pkgs.writeShellScript "start-pomodoro.sh" ''
            ${makoctl} mode -a sticky &
            ${swaymsg} 'gaps inner all set 0' &
            if [ -e "$XDG_RUNTIME_DIR/gnome-pomodoro-idle" ]; then
              (${gnome-pomodoro} --pause && echo 1 > "$XDG_RUNTIME_DIR/gnome-pomodoro-paused") &
            else
              ${swaymsg} 'bar mode hide' &
            fi

            ${pkgs.coreutils}/bin/rm -f "$XDG_RUNTIME_DIR/gnome-pomodoro-paused"
            ${pkgs.coreutils}/bin/rm -f "$XDG_RUNTIME_DIR/gnome-pomodoro-break"
            ${pkgs.coreutils}/bin/sleep ${timeout} && ${makoctl} mode | ${pkgs.gnugrep}/bin/grep -q sticky && ${makoctl} mode -a invisible
          '');
        };

      "org/gnome/pomodoro/plugins/actions/action1" = {
        name = "Pause Pomodoro";
        states = [ "pomodoro" ];
        triggers = [ "pause" ];
        command = toString (pkgs.writeShellScript "pause-pomodoro.sh" ''
          ${swaymsg} 'bar mode dock' &
          ${pkgs.coreutils}/bin/touch "$XDG_RUNTIME_DIR/gnome-pomodoro-paused"
        '');
      };

      "org/gnome/pomodoro/plugins/actions/action2" = {
        name = "Resume Pomodoro";
        states = [ "pomodoro" ];
        triggers = [ "resume" ];
        command = toString (pkgs.writeShellScript "resume-pomodoro.sh" ''
          ${swaymsg} 'bar mode hide' &
          ${pkgs.coreutils}/bin/rm -f "$XDG_RUNTIME_DIR/gnome-pomodoro-paused"
        '');
      };

      "org/gnome/pomodoro/plugins/actions/action3" = {
        name = "End Pomodoro";
        states = [ "pomodoro" ];
        triggers = [ "complete" "skip" ];
        command = toString (pkgs.writeShellScript "end-pomodoro.sh" ''
          ${makoctl} mode -r sticky -r invisible &
          ${swaymsg} 'gaps inner all set 10' &
          ${swaymsg} 'bar mode dock' &
          ${pkgs.coreutils}/bin/rm -f "$XDG_RUNTIME_DIR/gnome-pomodoro-paused"
          ${pkgs.coreutils}/bin/touch "$XDG_RUNTIME_DIR/gnome-pomodoro-break"
        '');
      };
    })
  ];

  wayland.windowManager.sway.config =
    let
      cfg = config.wayland.windowManager.sway.config;
    in
    {
      keybindings = lib.mkOptionDefault {
        "${cfg.modifier}+p" = "exec ${gnome-pomodoro} --start-stop";
        "${cfg.modifier}+Shift+p" = "exec ${gnome-pomodoro} --pause-resume";
      };

      startup = [{ command = "${gnome-pomodoro} --no-default-window --stop"; }];
    };

  services.swayidle.timeouts = [
    {
      timeout = 60;
      command = toString (pkgs.writeShellScript "gnome-pomodoro-idle" ''
       if [ ! -e "$XDG_RUNTIME_DIR/gnome-pomodoro-paused" ] && [ ! -e "$XDG_RUNTIME_DIR/gnome-pomodoro-break" ]; then
         ${gnome-pomodoro} --pause && echo 1 > "$XDG_RUNTIME_DIR/gnome-pomodoro-paused"
       fi

       ${pkgs.coreutils}/bin/touch "$XDG_RUNTIME_DIR/gnome-pomodoro-idle"
     '');

      resumeCommand = toString (pkgs.writeShellScript "gnome-pomodoro-idle-resume" ''
        if [ -s "$XDG_RUNTIME_DIR/gnome-pomodoro-paused" ]; then
          ${gnome-pomodoro} --resume &
        fi

        ${pkgs.coreutils}/bin/rm -f "$XDG_RUNTIME_DIR/gnome-pomodoro-idle"
      '');
    }
  ];
}

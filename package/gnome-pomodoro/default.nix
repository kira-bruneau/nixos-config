{ lib, config, pkgs, ... }:

let
  package = pkgs.gnome.pomodoro;
  gnome-pomodoro = "${package}/bin/gnome-pomodoro";
  cfg = config.programs.gnome-pomodoro;
  types = lib.types;
in
{
  options = {
    programs.gnome-pomodoro = {
      onstart = lib.mkOption { type = types.listOf types.str; };
      onend = lib.mkOption { type = types.listOf types.str; };
      onpause = lib.mkOption { type = types.listOf types.str; };
      onresume = lib.mkOption { type = types.listOf types.str; };
    };
  };

  config = {
    home.packages = [ package ];

    dconf.settings = {
      "org/gnome/pomodoro/preferences" = {
        enabled-plugins = [
          "dark-theme"
          "sounds"
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

      "org/gnome/pomodoro/plugins/actions/action0" = {
        name = "Start Pomodoro";
        states = [ "pomodoro" ];
        triggers = [ "start" ];
        command = toString (pkgs.writeShellScript "start-pomodoro" ''
          ${builtins.concatStringsSep " &\n" cfg.onstart} &
          if [ -e "$XDG_RUNTIME_DIR/gnome-pomodoro-idle" ]; then
            (${gnome-pomodoro} --pause && echo 1 > "$XDG_RUNTIME_DIR/gnome-pomodoro-paused") &
          fi
          ${pkgs.coreutils}/bin/rm -f "$XDG_RUNTIME_DIR/gnome-pomodoro-paused"
          ${pkgs.coreutils}/bin/rm -f "$XDG_RUNTIME_DIR/gnome-pomodoro-break"
        '');
      };

      "org/gnome/pomodoro/plugins/actions/action1" = {
        name = "End Pomodoro";
        states = [ "pomodoro" ];
        triggers = [ "complete" "skip" ];
        command = toString (pkgs.writeShellScript "end-pomodoro" ''
          ${builtins.concatStringsSep " &\n" cfg.onend} &
          ${pkgs.coreutils}/bin/rm -f "$XDG_RUNTIME_DIR/gnome-pomodoro-paused"
          ${pkgs.coreutils}/bin/touch "$XDG_RUNTIME_DIR/gnome-pomodoro-break"
        '');
      };

      "org/gnome/pomodoro/plugins/actions/action2" = {
        name = "Pause Pomodoro";
        states = [ "pomodoro" ];
        triggers = [ "pause" ];
        command = toString (pkgs.writeShellScript "pause-pomodoro" ''
          ${builtins.concatStringsSep " &\n" cfg.onpause} &
          ${pkgs.coreutils}/bin/touch "$XDG_RUNTIME_DIR/gnome-pomodoro-paused"
        '');
      };

      "org/gnome/pomodoro/plugins/actions/action3" = {
        name = "Resume Pomodoro";
        states = [ "pomodoro" ];
        triggers = [ "resume" ];
        command = toString (pkgs.writeShellScript "resume-pomodoro" ''
          ${builtins.concatStringsSep " &\n" cfg.onresume} &
          ${pkgs.coreutils}/bin/rm -f "$XDG_RUNTIME_DIR/gnome-pomodoro-paused"
        '');
      };
    };

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
  };
}

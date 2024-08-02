{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) types;

  exe = lib.getExe config.programs.gnome-pomodoro.package;

  cfg = config.programs.gnome-pomodoro-swayidle;
in
{
  options = {
    programs.gnome-pomodoro-swayidle = {
      onstart = lib.mkOption {
        type = types.listOf types.str;
        default = [ ];
      };

      onend = lib.mkOption {
        type = types.listOf types.str;
        default = [ ];
      };

      onpause = lib.mkOption {
        type = types.listOf types.str;
        default = [ ];
      };

      onresume = lib.mkOption {
        type = types.listOf types.str;
        default = [ ];
      };
    };
  };

  config = {
    programs.gnome-pomodoro.actions = [
      {
        name = "Start Pomodoro";
        states = [ "pomodoro" ];
        triggers = [ "start" ];
        command = toString (
          pkgs.writeShellScript "start-pomodoro" ''
            ${lib.concatMapStrings (cmd: "${cmd} &\n") cfg.onstart}
            if [ -e "$XDG_RUNTIME_DIR/gnome-pomodoro-idle" ]; then
              (${exe} --pause && echo 1 > "$XDG_RUNTIME_DIR/gnome-pomodoro-paused") &
            fi
            ${pkgs.coreutils}/bin/rm -f "$XDG_RUNTIME_DIR/gnome-pomodoro-paused"
            ${pkgs.coreutils}/bin/rm -f "$XDG_RUNTIME_DIR/gnome-pomodoro-break"
          ''
        );
      }
      {
        name = "End Pomodoro";
        states = [ "pomodoro" ];
        triggers = [
          "complete"
          "skip"
        ];
        command = toString (
          pkgs.writeShellScript "end-pomodoro" ''
            ${lib.concatMapStrings (cmd: "${cmd} &\n") cfg.onend}
            ${pkgs.coreutils}/bin/rm -f "$XDG_RUNTIME_DIR/gnome-pomodoro-paused"
            ${pkgs.coreutils}/bin/touch "$XDG_RUNTIME_DIR/gnome-pomodoro-break"
          ''
        );
      }
      {
        name = "Pause Pomodoro";
        states = [ "pomodoro" ];
        triggers = [ "pause" ];
        command = toString (
          pkgs.writeShellScript "pause-pomodoro" ''
            ${lib.concatMapStrings (cmd: "${cmd} &\n") cfg.onpause}
            ${pkgs.coreutils}/bin/touch "$XDG_RUNTIME_DIR/gnome-pomodoro-paused"
          ''
        );
      }
      {
        name = "Resume Pomodoro";
        states = [ "pomodoro" ];
        triggers = [ "resume" ];
        command = toString (
          pkgs.writeShellScript "resume-pomodoro" ''
            ${lib.concatMapStrings (cmd: "${cmd} &\n") cfg.onresume}
            ${pkgs.coreutils}/bin/rm -f "$XDG_RUNTIME_DIR/gnome-pomodoro-paused"
          ''
        );
      }
    ];

    services.swayidle.timeouts = [
      {
        timeout = 60;
        command = toString (
          pkgs.writeShellScript "gnome-pomodoro-idle" ''
            if [ ! -e "$XDG_RUNTIME_DIR/gnome-pomodoro-paused" ] && [ ! -e "$XDG_RUNTIME_DIR/gnome-pomodoro-break" ]; then
              ${exe} --pause && echo 1 > "$XDG_RUNTIME_DIR/gnome-pomodoro-paused"
            fi

            ${pkgs.coreutils}/bin/touch "$XDG_RUNTIME_DIR/gnome-pomodoro-idle"
          ''
        );

        resumeCommand = toString (
          pkgs.writeShellScript "gnome-pomodoro-idle-resume" ''
            if [ -s "$XDG_RUNTIME_DIR/gnome-pomodoro-paused" ]; then
              ${exe} --resume &
            fi

            ${pkgs.coreutils}/bin/rm -f "$XDG_RUNTIME_DIR/gnome-pomodoro-idle"
          ''
        );
      }
    ];
  };
}

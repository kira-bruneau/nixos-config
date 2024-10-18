{
  config,
  lib,
  pkgs,
  ...
}:

{
  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      set fish_greeting # Disable greeting
    '';
  };

  programs.bash.initExtra = lib.mkAfter ''
    if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} && ''${SHLVL} == 1 ]]
    then
      shopt -q login_shell && login_options=('--login') || login_options=()
      exec ${lib.getExe config.programs.fish.package} "''${login_options[@]}"
    fi
  '';
}

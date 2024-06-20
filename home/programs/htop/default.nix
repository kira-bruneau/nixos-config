{
  programs.htop = {
    enable = true;
    settings = {
      hide_userland_threads = 1;
      highlight_base_name = 1;
      show_cpu_temperature = 1;
      show_program_path = 0;
    };
  };

  # https://github.com/nix-community/home-manager/issues/4947
  xdg.configFile."htop/htoprc".force = true;
}

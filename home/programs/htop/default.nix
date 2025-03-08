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
}

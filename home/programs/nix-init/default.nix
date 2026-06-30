{
  programs.nix-init = {
    enable = true;
    settings = {
      maintainers = [ "kira-bruneau" ];
      commit = true;
    };
  };
}

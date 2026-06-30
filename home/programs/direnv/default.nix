{
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    config.whitelist.prefix = [
      "~/Dev/archive"
      "~/Dev/private"
      "~/Dev/public"
    ];
  };
}

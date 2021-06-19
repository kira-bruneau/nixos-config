{ ... }:

{
  programs.direnv = {
    enable = true;
    enableFishIntegration = false;
    enableZshIntegration = false;
    nix-direnv = {
      enable = true;
      enableFlakes = true;
    };
  };
}

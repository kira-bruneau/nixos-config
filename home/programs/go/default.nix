{ config, ... }:

{
  programs.go = {
    enable = true;
    env.GOPATH = "${config.home.homeDirectory}/.local/state/go";
  };
}

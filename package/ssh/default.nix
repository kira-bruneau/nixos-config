{ config, ... }:

{
  programs.ssh.enable = true;

  home.file = {
    # TODO: Generate configuration from Nix
    ".ssh/config".source = ./config;

    # Manage known_hosts outside of home-manager (synced with Syncthing)
    ".ssh/known_hosts".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/.config/nixpkgs/package/ssh/known_hosts";
  };
}

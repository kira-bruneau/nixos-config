{ config, ... }:

{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;

    matchBlocks."*" = {
      # Keep connection alive
      serverAliveInterval = 60;
      serverAliveCountMax = 2;

      # Share SSH connections
      controlMaster = "auto";
      controlPath = "~/.ssh/master-%r@%n:%p";
      controlPersist = "5m";

      # Manage known_hosts outside of home-manager (synced with Syncthing)
      userKnownHostsFile = builtins.toString (
        config.lib.file.mkOutOfStoreSymlink "${config.home.configDirectory}/programs/ssh/known_hosts"
      );

      extraOptions = {
        ConnectTimeout = "3";
      };
    };
  };
}

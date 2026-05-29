{ config, ... }:

{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    settings."*" = {
      ConnectTimeout = 3;

      # Keep connection alive
      ServerAliveInterval = 60;
      ServerAliveCountMax = 2;

      # Share SSH connections
      ControlMaster = "auto";
      ControlPath = "~/.ssh/master-%r@%n:%p";
      ControlPersist = "5m";

      # Manage known_hosts outside of home-manager (synced with Syncthing)
      UserKnownHostsFile = toString (
        config.lib.file.mkOutOfStoreSymlink "${config.home.configDirectory}/programs/ssh/known_hosts"
      );
    };
  };
}

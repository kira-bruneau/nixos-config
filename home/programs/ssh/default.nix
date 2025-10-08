{ config, ... }:

{
  programs.ssh = {
    enable = true;

    matchBlocks."*" = {
      # Share SSH connections
      controlMaster = "auto";
      controlPersist = "5m";

      # Manage known_hosts outside of home-manager (synced with Syncthing)
      userKnownHostsFile = builtins.toString (
        config.lib.file.mkOutOfStoreSymlink "${config.home.configDirectory}/programs/ssh/known_hosts"
      );
    };

    extraConfig = ''
      ConnectTimeout 3
      ServerAliveInterval 60
      ServerAliveCountMax 2
    '';
  };
}

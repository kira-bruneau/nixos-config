{ config, ... }:

{
  programs.ssh = {
    enable = true;

    # Share SSH connections
    controlMaster = "auto";
    controlPersist = "5m";

    # Manage known_hosts outside of home-manager (synced with Syncthing)
    userKnownHostsFile = builtins.toString (
      config.lib.file.mkOutOfStoreSymlink "${config.home.configDirectory}/programs/ssh/known_hosts"
    );
  };
}

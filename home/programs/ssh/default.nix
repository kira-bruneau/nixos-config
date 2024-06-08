{ config, ... }:

{
  imports = [ ../../environments/config.nix ];

  programs.ssh = {
    enable = true;

    # Share SSH connections
    controlMaster = "auto";
    controlPersist = "5m";

    # Manage known_hosts outside of home-manager (synced with Syncthing)
    userKnownHostsFile = builtins.toString (
      config.lib.file.mkOutOfStoreSymlink "${config.home.configDirectory}/programs/ssh/known_hosts"
    );

    matchBlocks = {
      "amethyst".user = "kira";
      "amethyst.lan".user = "kira";
      "aurora".user = "kira";
      "aurora.lan".user = "kira";
      "jackflix".user = "jakira";
      "jackflix.lan".user = "jakira";
      "jakira.space".user = "kira";
      "quartz".user = "kira";
      "quartz.lan".user = "kira";
      "steamdeck".user = "jakira";
      "steamdeck.lan".user = "jakira";
    };
  };
}

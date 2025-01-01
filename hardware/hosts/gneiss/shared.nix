{ config, ... }:

{
  nix = {
    buildMachines = builtins.filter (machine: machine.hostName != config.networking.hostName) [
      {
        protocol = "ssh-ng";
        sshUser = "builder";
        hostName = "gneiss";
        maxJobs = 4; # 4 cores
        speedFactor = 1296; # MHz, max "boost" clock speed
        systems = [ "aarch64-linux" ];
      }
    ];

    settings.trusted-public-keys = [
      "rocky:JuZH5XWnnaZj03DtceTDe5xEHHMgcn0+vzToW1Z3Pgg="
    ];
  };

  networking.hosts."100.64.0.10" = [ "gneiss" ];

  programs.ssh.knownHosts.gneiss.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINUGzRm127zLH3ZQxwGTFmWyggeu7uwoaMnCGUdCq0fO";
}

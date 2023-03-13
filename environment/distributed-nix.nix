{ config, ... }:

{
  nix = {
    distributedBuilds = true;

    buildMachines = builtins.filter
      (machine: machine.hostName != config.networking.hostName)
      [
        {
          hostName = "atlantis";
          protocol = "ssh-ng";
          sshUser = "nix-ssh";
          systems = [ "x86_64-linux" "i686-linux" ];
          maxJobs = 12; # 6 cores, each with 2 threads
          speedFactor = 3900; # MHz, max "boost" clock speed
          supportedFeatures = [ "big-parallel" "kvm" ];
        }
      ];

    settings = {
      substituters = builtins.concatMap
        (hostName:
          if hostName != config.networking.hostName
          then [ "ssh-ng://nix-ssh@${hostName}" ]
          else [ ])
        [
          "atlantis"
          "aurora"
        ];

      trusted-public-keys = [
        "atlantis-1:ZqUz/vZRVnnDhElFo9yuuw7Tclny356pJ68bG5JBYOM="
        "aurora-1:B5Zp+k4JzGIXTC30BYTxfGsWgnlmA2nY6qlrx1uTc6s="
      ];
    };

    extraOptions = ''
      builders-use-substitutes = true
      fallback = true
    '';
  };

  programs.ssh = {
    knownHosts = {
      atlantis.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPBPD66/axLTeJHQ+lLmOSJT2VQyESnk5VRr7Rkx4BET";
      aurora.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDG2wKEtIS2KuoXr4uNAzwTLvkyjrLX9zonE3pZB2pdH";
    };

    # NOTE: This assumes the ssh service is enabled. Maybe should
    # automatically run ssh-keygen if not enabled?
    extraConfig = ''
      IdentityFile /etc/ssh/ssh_host_ed25519_key
    '';
  };
}

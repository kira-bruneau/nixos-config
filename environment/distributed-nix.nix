{ config, pkgs, ... }:

{
  nix = {
    distributedBuilds = true;

    buildMachines = builtins.filter
      (machine: machine.hostName != config.networking.hostName)
      [
        {
          hostName = "quartz";
          protocol = "ssh-ng";
          sshUser = "builder";
          systems = [ "x86_64-linux" "i686-linux" ];
          maxJobs = 12; # 6 cores, each with 2 threads
          speedFactor = 3900; # MHz, max "boost" clock speed
          supportedFeatures = [ "big-parallel" "kvm" ];
        }
      ];

    settings = {
      builders-use-substitutes = true;
      fallback = true;
      secret-key-files = "/var/lib/nix-daemon/private-key";
    };
  };

  systemd.services.nix-daemon = {
    serviceConfig.StateDirectory = "nix-daemon";
    preStart = ''(
      cd /var/lib/nix-daemon
      if [ ! -e private-key ] || [ ! -e public-key ]; then
        ${pkgs.nix}/bin/nix-store --generate-binary-cache-key \
          ${config.networking.hostName} private-key public-key
      fi
    )'';
  };

  programs.ssh = {
    knownHosts = {
      quartz.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPBPD66/axLTeJHQ+lLmOSJT2VQyESnk5VRr7Rkx4BET";
      aurora.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDG2wKEtIS2KuoXr4uNAzwTLvkyjrLX9zonE3pZB2pdH";
    };

    # NOTE: This assumes the ssh service is enabled. Maybe should
    # automatically run ssh-keygen if not enabled?
    extraConfig = ''
      IdentityFile /etc/ssh/ssh_host_ed25519_key
    '';
  };
}

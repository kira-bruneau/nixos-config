{ inputs, config, pkgs, ... }:

{
  system.nixos.label = builtins.concatStringsSep "-"
    (config.system.nixos.tags ++ [
      (if inputs.self ? shortRev
       then inputs.self.shortRev
       else inputs.self.dirtyShortRev)
    ]);

  nix = {
    # Pin nixpkgs in flake registry
    registry.nixpkgs.flake = inputs.nixpkgs;

    # Pin nixpkgs channel (for backwards compatibility with nix2 cli)
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];

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
      auto-optimise-store = true;
      builders-use-substitutes = true;
      experimental-features = [ "nix-command" "flakes" ];
      keep-going = true;
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
      aurora.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDG2wKEtIS2KuoXr4uNAzwTLvkyjrLX9zonE3pZB2pdH";
      quartz.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPBPD66/axLTeJHQ+lLmOSJT2VQyESnk5VRr7Rkx4BET";
    };

    extraConfig = ''
      IdentityFile /etc/ssh/ssh_host_ed25519_key
    '';
  };

  programs.git.enable = true;
}

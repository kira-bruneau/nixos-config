{ inputs, config, pkgs, ... }:

{
  imports = [
    ../program/ssh.nix
  ];

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
      trusted-public-keys = [
        "aurora:PkeJpeCTFE3gprtNpxCW0EqbVwg0wFgvpHFq3Hj0Wlc="
        "quartz:5ihtRHWq3L8mirx1UEy2uDAkb12NQUN+t+OT4NAnEp8="
      ];
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

  programs.git.enable = true;
}

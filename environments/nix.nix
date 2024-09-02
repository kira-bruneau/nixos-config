{
  inputs,
  config,
  pkgsUnstable,
  ...
}:

{
  system.nixos.label = builtins.concatStringsSep "-" (
    config.system.nixos.tags
    ++ [
      (
        if inputs.self ? shortRev then
          inputs.self.shortRev
        else if inputs.self ? dirtyShortRev then
          inputs.self.dirtyShortRev
        else
          "dirty"
      )
    ]
  );

  nix = {
    package = pkgsUnstable.lix;

    # Pin nixpkgs in flake registry
    registry = {
      nixpkgs.flake = inputs.nixpkgs;
      nixpkgs-unstable.flake = inputs.nixpkgs-unstable;
    };

    # Pin nixpkgs channel (for backwards compatibility with nix2 cli)
    nixPath = [
      "nixpkgs=${inputs.nixpkgs}"
      "nixpkgs-unstable=${inputs.nixpkgs}"
    ];

    distributedBuilds = true;
    buildMachines = builtins.filter (machine: machine.hostName != config.networking.hostName) [
      {
        hostName = "quartz";
        protocol = "ssh-ng";
        sshUser = "builder";
        systems = [
          "x86_64-linux"
          "i686-linux"
        ];
        maxJobs = 12; # 6 cores, each with 2 threads
        speedFactor = 3400; # MHz, max "boost" clock speed
        supportedFeatures = [
          "big-parallel"
          "kvm"
        ];
      }
    ];

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
      persistent = true;
    };

    settings = {
      auto-optimise-store = true;
      builders-use-substitutes = true;
      connect-timeout = 3;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      keep-going = true;
      secret-key-files = "/var/lib/nix/private-key";
      trusted-public-keys = [
        "amethyst:wWzYeKRMtWixW1rMNwf4jG+wWPUwRMEHCEB5WKixoes="
        "aurora:PkeJpeCTFE3gprtNpxCW0EqbVwg0wFgvpHFq3Hj0Wlc="
        "jackflix:CtqkVpen2v2aTo4xs8LsmvZcPeke0ewYAtjLRbh+Vvw="
        "quartz:5ihtRHWq3L8mirx1UEy2uDAkb12NQUN+t+OT4NAnEp8="
        "steamdeck:BcQXU+d7+azmiE/6YBWs/OJpIYlhcuTcpTU2j7+Zxb8="
      ];
    };

    extraOptions = ''
      !include /var/lib/nix/access-tokens
    '';
  };

  systemd.services.nix-daemon = {
    serviceConfig.StateDirectory = "nix";
    preStart = ''
      (
        cd /var/lib/nix
        if [ ! -e private-key ] || [ ! -e public-key ]; then
          ${config.nix.package}/bin/nix-store --generate-binary-cache-key \
            ${config.networking.hostName} private-key public-key
        fi
      )
    '';
  };

  programs.git.enable = true;
}

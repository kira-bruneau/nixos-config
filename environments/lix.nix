{
  inputs,
  config,
  lib,
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

  nixpkgs.overlays = [
    (final: prev: {
      nix = final.lix;
    })
  ];

  nix = {
    # Pin flake inputs to the registry
    registry = (builtins.mapAttrs (_name: input: { flake = input; }) inputs) // {
      n.flake = inputs.nixpkgs;
      u.flake = inputs.nixpkgs-unstable;
    };

    # Pin flake inputs as channels (for backwards compatibility with nix2 cli)
    nixPath = lib.mapAttrsToList (name: input: "${name}=${input}") inputs;

    distributedBuilds = true;

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
      persistent = true;
    };

    settings = {
      allowed-users = [
        "@wheel"
        "builder"
      ];
      auto-optimise-store = true;
      builders-use-substitutes = true;
      connect-timeout = 3;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      keep-going = true;
      secret-key-files = "/var/lib/nix/private-key";
    };

    extraOptions = ''
      !include /var/lib/nix/access-tokens
    '';
  };

  systemd = {
    services.nix-daemon = {
      serviceConfig = {
        Slice = "system-nix.slice";
        StateDirectory = "nix";
      };

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

    slices.system-nix.sliceConfig = lib.mkIf config.systemd.oomd.enable {
      ManagedOOMMemoryPressure = "kill";
      ManagedOOMMemoryPressureLimit = lib.mkDefault "80%";
    };
  };

  programs.git.enable = true;
}

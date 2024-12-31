{
  inputs,
  config,
  lib,
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

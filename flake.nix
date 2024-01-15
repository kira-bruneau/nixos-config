{
  description = "My NixOS configuration";

  inputs = {
    systems.url = "github:nix-systems/default";

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";

    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    kira-nur = {
      url = "gitlab:kira-bruneau/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { systems, nixpkgs, nixos-generators, ... } @ inputs:
    let
      eachSystem = nixpkgs.lib.genAttrs (import systems);
      lib = nixpkgs.lib;
      hosts = builtins.listToAttrs
        (builtins.map
          (file:
            let
              name = lib.removeSuffix ".nix" file;
            in
            {
              inherit name;
              value = {
                imports = [ ./hosts/${file} ];
                networking.hostName = name;
              };
            })
          (builtins.attrNames (builtins.readDir ./hosts)));
    in
    {
      nixosConfigurations = builtins.listToAttrs
        (builtins.concatMap
          (hostName:
            if builtins.pathExists ./hardware/hosts/${hostName}
            then [
              {
                name = hostName;
                value = lib.nixosSystem {
                  specialArgs = { inherit inputs; };
                  modules = [
                    ./hardware/hosts/${hostName}
                    hosts.${hostName}
                  ];
                };
              }
            ]
            else [ ]
          )
          (builtins.attrNames hosts));

      packages = eachSystem (system:
        builtins.foldl'
          (packages: hostName:
            let
              hostModule = hosts.${hostName};
              pkgs = nixpkgs.legacyPackages.${system};
            in
            packages // {
              "${hostName}/install-iso" = nixos-generators.nixosGenerate {
                inherit system;
                format = "install-iso";
                specialArgs = { inherit inputs; };
                modules = [
                  hostModule
                  {
                    # /etc/nixos is seeded with the contents of this flake
                    installer.cloneConfig = false;

                    # Disable ZFS support, it may not be compatible
                    # with the configured kernel version
                    nixpkgs.overlays = [
                      (final: super: {
                        zfs = super.zfs.overrideAttrs (_: {
                          meta.platforms = [ ];
                        });
                      })
                    ];

                    # Disable wpa_supplicant (I use iwd)
                    networking.wireless.enable = false;

                    # Resolve conflict between install iso config and my host configs
                    services.openssh.settings.PermitRootLogin = lib.mkForce "no";
                  }
                ];
              };
              "${hostName}/vm" = nixos-generators.nixosGenerate {
                inherit system;
                format = "vm";
                specialArgs = { inherit inputs; };
                modules = [
                  hostModule
                  {
                    virtualisation = {
                      memorySize = 1024 * 4;
                      qemu.options = [
                        "-smp $(${pkgs.coreutils}/bin/nproc)"
                        "-device virtio-vga-gl"
                        "-display gtk,gl=on,grab-on-hover=on"
                      ];
                    };

                    systemd.network.networks.eth0 = {
                      matchConfig.Name = "eth0";
                      networkConfig.DHCP = "yes";
                    };

                    environment.sessionVariables = {
                      WLR_NO_HARDWARE_CURSORS = "1";
                    };

                    environment.etc."sway/config.d/io.conf".text = ''
                      output "*" scale 2
                    '';

                    services.openssh.enable = lib.mkForce false;
                    services.syncthing.enable = lib.mkForce false;
                    services.tailscale.enable = lib.mkForce false;
                  }
                ];
              };
            })
          { }
          (builtins.attrNames hosts)
      );
    };
}

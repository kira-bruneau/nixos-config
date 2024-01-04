{
  description = "My NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
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
  };

  outputs = { nixpkgs, nixos-generators, ... } @ inputs:
    let
      lib = nixpkgs.lib;

      hostsDir = ./host;

      hosts = builtins.listToAttrs
        (builtins.map
          (host: {
            name = host;
            value = hostsDir + "/${host}";
          })
          (builtins.attrNames (builtins.readDir hostsDir)));
    in
    {
      nixosConfigurations = builtins.mapAttrs
        (host: path:
          lib.nixosSystem {
            specialArgs = { inherit inputs; };
            modules = [
              {
                networking.hostName = host;
              }
              (path + "/hardware")
              path
            ];
          })
        hosts;

      packages = builtins.foldl'
        (packages: host:
          let
            path = hosts.${host};
            config = (import (path + "/hardware/generated.nix") {
              pkgs = nixpkgs.legacyPackages.${system};
              modulesPath = nixpkgs + "/modules";
              inherit config lib;
            });

            system = config.nixpkgs.hostPlatform.content;
          in
            packages // {
              ${system} = (packages.${system} or {}) // {
                "${host}/install-iso" = nixos-generators.nixosGenerate {
                  pkgs = nixpkgs.legacyPackages.${system};
                  format = "install-iso";
                  specialArgs = { inherit inputs; };
                  modules = [
                    {
                      networking.hostName = host;
                    }
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
                    path
                  ];
                };
                "${host}/vm" = nixos-generators.nixosGenerate {
                  pkgs = nixpkgs.legacyPackages.${system};
                  format = "vm";
                  specialArgs = { inherit inputs; };
                  modules = [
                    {
                      networking.hostName = host;
                    }
                    {
                      virtualisation = {
                        memorySize = 1024 * 12;
                        qemu.options = [
                          "-device virtio-vga-gl"
                          "-display gtk,gl=on"
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
                    }
                    path
                  ];
                };
              };
            })
        {}
        (builtins.attrNames hosts);
    };
}

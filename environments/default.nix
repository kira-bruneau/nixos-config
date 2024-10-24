{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    inputs.home-manager.nixosModules.default
    ../cachix.nix
    ../modules
    ../services/dnscrypt.nix
    ../services/systemd-networkd.nix
    ./custom-nixos-generate-config.nix
    ./locale
    ./nix.nix
  ];

  config = {
    environment = {
      defaultPackages = [ ];
      systemPackages = with pkgs; [
        iftop
        nethogs
      ];
    };

    users.mutableUsers = false;

    home-manager = {
      useUserPackages = true;
      useGlobalPkgs = true;
      sharedModules = [ ../home/environments/default.nix ];
      users.root = {
        home.stateVersion = "24.05";
      };
    };

    # Unbind network-online from multi-user.target to speed up boot
    # https://github.com/NixOS/nixpkgs/pull/282795
    systemd.targets.network-online.wantedBy = lib.mkForce [ ];

    # Prefer nftables over iptables
    networking = {
      nftables.enable = true;
      firewall.trustedInterfaces = lib.mkIf config.services.tailscale.enable [
        config.services.tailscale.interfaceName
      ];
    };

    # Use dbus-broker since it's designed for high performance
    services.dbus.implementation = "broker";

    nixpkgs.config = {
      allowUnfreePredicate =
        pkg:
        builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
          "2ship2harkinian"
          "anytype"
          "anytype-heart"
          "aspell-dict-en-science"
          "broadcom-sta"
          "clonehero"
          "clonehero-unwrapped"
          "data.zip" # (vvvvvv)
          "discord"
          "minecraft-server"
          "nvidia-settings"
          "nvidia-x11"
          "sm64ex"
          "steam"
          "steam-jupiter-original"
          "steam-original"
          "steam-unwrapped"
          "steamdeck-hw-theme"
          "unrar"
          "vvvvvv"
        ];

      permittedInsecurePackages = [ "electron-29.4.6" ];
    };
  };
}

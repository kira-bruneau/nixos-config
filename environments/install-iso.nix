{ lib, modulesPath, ... }:

{
  imports = [
    "${toString modulesPath}/installer/cd-dvd/installation-cd-base.nix"
    ./stateless.nix
  ];

  # Resolve conflict between install iso config and my host configs
  boot.loader.timeout = lib.mkImageMediaOverride 10;

  # Disable impermanence
  environment.persistence = lib.mkImageMediaOverride {};

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
  services.openssh.settings.PermitRootLogin = lib.mkImageMediaOverride "no";
}

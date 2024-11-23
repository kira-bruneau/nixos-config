{
  config,
  lib,
  modulesPath,
  ...
}:

{
  imports = [
    "${toString modulesPath}/installer/cd-dvd/installation-cd-base.nix"
    ./stateless.nix
  ];

  # Always include redistributable firmware, just in case something
  # wasn't covered for the specific host
  hardware.enableRedistributableFirmware = lib.mkImageMediaOverride true;

  # Resolve conflict between install iso config and my host configs
  boot.loader.timeout = lib.mkImageMediaOverride 10;

  # Disable impermanence
  environment.persistence = lib.mkImageMediaOverride { };

  # /etc/nixos is seeded with the contents of this flake
  installer.cloneConfig = false;

  # Disable ZFS support, it may not be compatible
  # with the configured kernel version
  nixpkgs.overlays = [
    (final: prev: {
      zfs = prev.zfs.overrideAttrs (_: {
        meta.platforms = [ ];
      });
    })
  ];

  # Disable wpa_supplicant (I use iwd)
  networking.wireless.enable = false;

  # Resolve conflict between install iso config and my host configs
  services.getty.autologinUser = lib.mkImageMediaOverride config.users.defaultUser;

  # Resolve conflict between install iso config and my host configs
  services.openssh.settings.PermitRootLogin = lib.mkImageMediaOverride "no";
}

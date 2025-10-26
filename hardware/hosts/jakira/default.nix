{
  lib,
  modulesPath,
  ...
}:

{
  imports = [
    "${toString modulesPath}/virtualisation/digital-ocean-config.nix"
    ./firefox-syncserver-proxy.nix
    ./foundry-vtt-proxy.nix
    ./synapse-proxy.nix
  ];

  # FIXME: Root partition uses tmpfs, which causes growpart to fail
  boot.growPartition = lib.mkForce false;

  fileSystems = {
    "/" = lib.mkForce {
      device = "none";
      fsType = "tmpfs";
      options = [
        "defaults"
        "size=3G"
        "mode=755"
      ];
    };

    "/persist" = {
      device = "/dev/disk/by-label/nixos";
      neededForBoot = true;
      autoResize = true;
      fsType = "ext4";
    };
  };

  environment.persistence."/persist".directories = [
    "/boot"
    "/home"
    "/nix"
  ];

  swapDevices = [
    {
      device = "/persist/swapfile";
      size = 2000;
    }
  ];

  zramSwap.enable = true;

  services.tailscale.enable = false;
}

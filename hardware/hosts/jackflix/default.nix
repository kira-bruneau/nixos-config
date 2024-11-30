{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:

{
  imports =
    [
      ../../drivers/logitech-wireless.nix
      ../../environments/cec
      ../../environments/gaming.nix
      ../../environments/laptop.nix
    ]
    ++ (with inputs.nixos-hardware.nixosModules; [
      common-cpu-intel-cpu-only
      common-gpu-nvidia-nonprime
      common-pc-laptop-hdd
    ]);

  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.legacy_470;

    # Modesetting is required for wayland
    modesetting.enable = true;
  };

  boot = {
    # Use the systemd-boot EFI boot loader
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      timeout = 0;
    };

    kernelPackages = pkgs.linuxPackages_latest;
  };

  disko.devices = {
    disk.main = {
      device = "/dev/disk/by-id/ata-HGST_HTS721075A9E630_JR12006QG0WSKE";
      content = {
        type = "gpt";
        partitions = {
          boot = {
            type = "EF00";
            label = "boot";
            size = "512M";
            content = {
              type = "filesystem";
              format = "vfat";
              extraArgs = [
                "-F"
                "32"
                "-n"
                "boot"
              ];
              mountpoint = "/boot";
              mountOptions = [ "noatime" ];
            };
          };
          nixos = {
            label = "nixos";
            size = "100%";
            content = {
              type = "btrfs";
              extraArgs = [
                "-L"
                "nixos"
              ];
              mountpoint = "/persist";
              mountOptions = [ "noatime" ];
            };
          };
        };
      };
    };

    nodev = {
      "/" = {
        fsType = "tmpfs";
        mountOptions = [
          "defaults"
          "mode=755"
        ];
      };
    };
  };

  environment.persistence."/persist".directories = [
    "/home"
    "/nix"
  ];

  services.hdapsd.enable = false;

  programs.captive-browser.interface = "wlp3s0";

  nixpkgs.config.nvidia.acceptLicense = true;

  services.auto-cpufreq.enable = lib.mkForce false;
}

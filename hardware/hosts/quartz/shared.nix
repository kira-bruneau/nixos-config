{ config, lib, ... }:

{
  nix = {
    buildMachines = lib.optional (config.networking.hostName != "quartz") {
      protocol = "ssh-ng";
      sshUser = "builder";
      hostName = "quartz";

      maxJobs = 12; # 6 cores, each with 2 threads
      speedFactor = 3400; # MHz, max "boost" clock speed

      systems = [
        "x86_64-linux"
        "i686-linux"
      ];

      supportedFeatures = [
        "big-parallel"
        "kvm"
      ];
    };

    settings.trusted-public-keys = [
      "quartz:5ihtRHWq3L8mirx1UEy2uDAkb12NQUN+t+OT4NAnEp8="
    ];
  };

  networking = {
    wireguard.interfaces.wg0 = {
      ips = lib.optional (config.networking.hostName == "quartz") "10.100.0.2/32";
      peers = lib.optional (config.networking.hostName != "quartz") {
        name = "quartz";
        publicKey = "s4KfpSZRL37pLbHieVAWFoVcpkzTP8LoMzlt/wQLQwk=";
        allowedIPs = [ "10.100.0.2/32" ];
      };
    };

    hosts."100.64.0.1" = lib.mkIf config.services.tailscale.enable [ "quartz" ];
  };

  programs.ssh.knownHosts.quartz.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPBPD66/axLTeJHQ+lLmOSJT2VQyESnk5VRr7Rkx4BET";

  services.syncthing.settings = {
    devices.quartz.id = "64ZDVRR-2DZB475-3IWMGU6-OU46FZQ-P44AXVI-OYI6TO3-VOCUVRT-L62KBAE";

    folders = {
      "Auth".devices = [ "quartz" ];
      "Dev".devices = [ "quartz" ];
      "Documents".devices = [ "quartz" ];
      "Pictures".devices = [ "quartz" ];
      "RSS".devices = [ "quartz" ];
      "Videos".devices = [ "quartz" ];
    };
  };
}

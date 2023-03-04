{ config, ... }:

{
  nix = {
    distributedBuilds = true;

    buildMachines = builtins.filter
      (machine: machine.hostName != config.networking.hostName)
      [
        {
          hostName = "atlantis";
          publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSVBCUEQ2Ni9heExUZUpIUStsTG1PU0pUMlZReUVTbms1VlJyN1JreDRCRVQgcm9vdEBhdGxhbnRpcwo";
          sshUser = "builder";
          sshKey = "/etc/ssh/ssh_host_ed25519_key";
          systems = [ "x86_64-linux" "i686-linux" ];
          maxJobs = 12; # 6 cores, each with 2 threads
          speedFactor = 3900; # MHz, max "boost" clock speed
        }
      ];

    extraOptions = ''
      builders-use-substitutes = true
    '';
  };
}

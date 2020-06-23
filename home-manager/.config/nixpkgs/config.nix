{
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball {
      url = "https://github.com/nix-community/NUR/archive/eefce92289098a0e77a5d62c08a5686789342064.tar.gz";
      sha256 = "1kv4b89mgwsxxm3p0kvxpl64s27y7nbq9qfixa4k825n4sx4nmkv";
    }) {
      inherit pkgs;
    };
  };

  allowUnfree = true;
}

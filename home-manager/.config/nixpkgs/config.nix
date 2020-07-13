{
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball {
      url = "https://github.com/nix-community/NUR/archive/1313ec5bc0d925b16eb58c40999af9044ac40d0d.tar.gz";
      sha256 = "1wzhzzi0213dnck2zqarajqa5fmxraqmlqvmj97mw1b4h84615w2";
    }) {
      inherit pkgs;
    };
  };

  allowUnfree = true;
}

{
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball {
      url = "https://github.com/nix-community/NUR/archive/676a5733e99cf03a6c3e0b634215f69118440ffe.tar.gz";
      sha256 = "1740yvn3p66sfhfia70s2fx2nxv71sq6vd85xnlblbkq3vr9d851";
    }) {
      inherit pkgs;
    };
  };

  allowUnfree = true;
}

{
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball {
      url = "https://github.com/nix-community/NUR/archive/78ded507ecebe72cd5634e772a1d9fd1f4bf15fb.tar.gz";
      sha256 = "1f455i1vsg7hj96lkwq7w9yrd3f8w8hydgjschm1af3f454w15sg";
    }) {
      inherit pkgs;
    };
  };

  allowUnfree = true;
}

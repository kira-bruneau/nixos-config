{
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball {
      url = "https://github.com/nix-community/NUR/archive/7fb601e8ea7e99a46478b55820456259608e91bb.tar.gz";
      sha256 = "133sby2bv2bqagrgjcih45cx8anjffigdyf7d26rhpp43rpzhmcr";
    }) {
      inherit pkgs;
    };
  };

  allowUnfree = true;
}

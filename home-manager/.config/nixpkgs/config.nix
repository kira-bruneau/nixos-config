{
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball {
      url = "https://github.com/nix-community/NUR/archive/d55b83d1ed97e6bc4eee12dd2982a6c4b9c572de.tar.gz";
      sha256 = "1dvdymdrw8sn2hsz2isvs37dm4s32ffqa4x64lj2i3d0bd5wfcav";
    }) {
      inherit pkgs;
    };
  };

  allowUnfree = true;
}

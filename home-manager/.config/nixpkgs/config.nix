{
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball {
      url = "https://github.com/nix-community/NUR/archive/2e8237c166a76c4b10dddeb41d37cd562fbc28fd.tar.gz";
      sha256 = "1ml1172pd3dm69h0274wkpyakzlh53gmrzfgjx03drql5rx0h7yd";
    }) {
      inherit pkgs;
    };
  };

  allowUnfree = true;

  permittedInsecurePackages = [
    "p7zip-16.02"
  ];
}

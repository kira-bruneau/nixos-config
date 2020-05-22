{
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball {
      url = "https://github.com/nix-community/NUR/archive/9942ee7eed49a8a4e819babda5bb91a433b758f7.tar.gz";
      sha256 = "01sxrw42ddalrgsflq6vlavqf36a3q7pbqgqd7zxim392mrbiaqj";
    }) {
      inherit pkgs;
    };
  };

  allowUnfree = true;

  permittedInsecurePackages = [
    "p7zip-16.02"
  ];
}

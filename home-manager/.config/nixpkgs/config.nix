{
  packageOverrides = pkgs: {
    nur = import <nur> {
      inherit pkgs;
    };
  };

  allowUnfree = true;
}

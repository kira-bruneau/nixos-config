{ config, ... }:

{
  imports = [
    ../../environment/config.nix
  ];

  home.file = {
    # Manage aspell dictionaries outside of home-manager (synced with Syncthing)
    ".aspell.en.prepl".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.configDirectory}/package/aspell/.aspell.en.prepl";

    ".aspell.en.pws".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.configDirectory}/package/aspell/.aspell.en.pws";
  };
}

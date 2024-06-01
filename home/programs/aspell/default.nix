{ config, ... }:

{
  imports = [ ../../environments/config.nix ];

  home.file = {
    # Manage aspell dictionaries outside of home-manager (synced with Syncthing)
    ".aspell.en.prepl".source = config.lib.file.mkOutOfStoreSymlink "${config.home.configDirectory}/programs/aspell/.aspell.en.prepl";

    ".aspell.en.pws".source = config.lib.file.mkOutOfStoreSymlink "${config.home.configDirectory}/programs/aspell/.aspell.en.pws";
  };
}

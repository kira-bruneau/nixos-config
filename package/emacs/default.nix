{ config, pkgs, ... }:

{
  imports = [
    ../aspell
    ../gpg
  ];

  home = {
    packages = with pkgs; [
      (pkgs.callPackage ./package {})
    ];

    sessionVariables.EDITOR = "emacseditor";
  };

  # Manage emacs config outside of home-manager while keeping track of the files in this git repo
  xdg.configFile.emacs.source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.configDirectory}/package/emacs/config";
}

{ config, pkgs, ... }:

{
  imports = [
    ../aspell
    ../gpg
  ];

  programs.emacs.package = pkgs.callPackage ./package {};

  home = {
    packages = with pkgs; [
      config.programs.emacs.package
    ];

    sessionVariables.EDITOR = "emacseditor";
  };

  # Manage emacs config outside of home-manager while keeping track of the files in this git repo
  xdg.configFile.emacs.source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.configDirectory}/package/emacs/config";
}

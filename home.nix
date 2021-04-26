{ config, pkgs, ... }:

{
  programs.emacs = with pkgs; with nur.repos.metadark; {
    enable = true;
    package = callPackage ./wrapper.nix {
      emacs = emacs-pgtk-native-comp;
      profile = pkgs.buildEnv {
        name = "emacs-profile";
        paths = [
          (aspellWithDicts (dicts: with dicts; [
            en
            en-computers
            en-science
          ]))
          bear
          cargo
          cargo-edit
          ccls
          cmake
          cmake-language-server
          diffutils
          fd
          fzf
          gcc
          gdb
          git
          imagemagick
          jdk
          libnotify
          nodejs
          nodePackages.bash-language-server
          nodePackages.typescript-language-server
          omnisharp-roslyn
          pandoc
          perl
          (python3.withPackages (pkgs: with pkgs; with nur.repos.metadark.python3Packages; [
            debugpy
            python-language-server
          ]))
          ripgrep
          rnix-lsp
          rust-analyzer
          rustc
          solargraph
          tectonic
          texlab
        ];
      };
    };
  };

  services.emacs.enable = true;
}

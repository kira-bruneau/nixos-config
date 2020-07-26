{ config, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.callPackage ./wrapper.nix {
      emacs = pkgs.emacs;
      profile = pkgs.buildEnv {
        name = "emacs-profile";
        paths = with pkgs; [
          (aspellWithDicts (dicts: with dicts; [
            en
            en-computers
            en-science
          ]))
          bear
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
          nodejs
          nodePackages.bash-language-server
          nodePackages.typescript
          nodePackages.typescript-language-server
          omnisharp-roslyn
          pandoc
          (python3.withPackages (pkgs: with pkgs; [
            debugpy
            python-language-server
          ]))
          ripgrep
          rnix-lsp
          rust-analyzer
          rustc
          tectonic
          texlab
        ];
      };
    };
  };

  services.emacs.enable = true;
}

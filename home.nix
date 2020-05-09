{ config, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.callPackage ./wrapper.nix {
      profile = pkgs.buildEnv {
        name = "emacs-profile";
        paths = with pkgs; [
          (aspellWithDicts (dicts: with dicts; [
            en
            en-computers
            en-science
          ]))
          ccls
          diffutils
          fd
          fzf
          gdb
          git
          imagemagick
          jdk
          nodejs
          nodePackages.bash-language-server
          nodePackages.typescript
          nodePackages.typescript-language-server
          pandoc
          (python3.withPackages(pkgs: with pkgs; [
            nur.repos.metadark.ptvsd
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

      makeWrapperArgs = [
        # Set default rust source path for rust-analyzer
        "--set-default RUST_SRC_PATH=${pkgs.rustPackages.rustPlatform.rustcSrc}"
      ];
    };
  };

  services.emacs.enable = true;
}

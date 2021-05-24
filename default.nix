{ pkgs }:

with pkgs;

callPackage ./wrapper.nix {
  emacs = emacsPgtkGcc;
  profile = buildEnv {
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
      (python3.withPackages (ps: with ps; [
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
}

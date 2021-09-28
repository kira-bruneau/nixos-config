{ pkgs }:

with pkgs;

callPackage ./wrapper.nix {
  emacs =
    if stdenv.hostPlatform.isDarwin
    then emacsGcc
    else emacsPgtkGcc;

  profile = buildEnv {
    name = "emacs-profile";
    paths = [
      (aspellWithDicts (dicts: with dicts; [
        en
        en-computers
        en-science
      ]))
      cargo
      bear
      tectonic
      cargo-edit
      ccls
      cmake
      diffutils
      direnv
      fd
      fzf
      gcc
      gdb
      git
      imagemagick
      libnotify
      lldb
      nodejs
      nodePackages.bash-language-server
      nodePackages.prettier
      nodePackages.typescript
      nodePackages.typescript-language-server
      pandoc
      perl
      (python38.withPackages (ps: with ps; [
        debugpy
      ] ++ lib.optionals (!stdenv.hostPlatform.isDarwin) [
        python-language-server
      ]))
      ripgrep
      rnix-lsp
      rust-analyzer
      rustc
      solargraph
      texlab
    ] ++ lib.optionals (!stdenv.hostPlatform.isDarwin) [
      # Currently doesn't built on Darwin
      cmake-language-server
      jdk
      omnisharp-roslyn
    ];
  };
}

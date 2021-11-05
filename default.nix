{ lib, pkgs }:

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
      go
      godef
      gopls
      imagemagick
      libnotify
      (lib.lowPrio lldb) # collides with six.py required by python-lsp-server
      nodejs
      nodePackages.bash-language-server
      nodePackages.prettier
      nodePackages.typescript
      nodePackages.typescript-language-server
      pandoc
      perl
      (python3.withPackages (ps: with ps; [
        debugpy
      ] ++ lib.optionals (!stdenv.hostPlatform.isDarwin) [
        python-lsp-server
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

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
      bear
      cargo
      cargo-edit
      cmake
      cmake-language-server
      coreutils
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
      jdk
      libnotify
      nodejs
      nodePackages.bash-language-server
      nodePackages.prettier
      nodePackages.typescript
      nodePackages.typescript-language-server
      pandoc
      perl
      (python3.withPackages (ps: with ps; [
        debugpy
        python-lsp-server
      ]))
      ripgrep
      rnix-lsp
      rust-analyzer
      rustc
      rustfmt
      solargraph
      tectonic
      texlab
    ] ++ lib.optionals (!stdenv.hostPlatform.isDarwin) [
      # Currently doesn't built on Darwin
      ccls
      (lib.lowPrio lldb) # collides with six.py required by python-lsp-server
      omnisharp-roslyn
    ];
  };
}

{ lib
, stdenv
, fetchpatch
, emacsGcc
, emacsPgtkGcc
, callPackage
, buildEnv
, aspellWithDicts
, bear
, cargo
, cargo-edit
, ccls
, cmake
, cmake-language-server
, coreutils
, diffutils
, direnv
, fd
, fzf
, gcc
, gdb
, git
, go
, godef
, gopls
, imagemagick
, jdk
, libnotify
, lldb
, nixpkgs-fmt
, nodejs
, nodePackages
, omnisharp-roslyn
, pandoc
, perl
, python3
, ripgrep
, rnix-lsp
, rust-analyzer
, rustc
, rustfmt
, solargraph
, tectonic
, texlab
}:

let
  emacs =
    if stdenv.hostPlatform.isDarwin
    then emacsGcc.overrideAttrs (attrs: {
      patches = attrs.patches ++ [
        # Removes titlebar
        # TODO: When Emacs 29 is released, replace with: (add-to-list 'default-frame-alist '(undecorated . t))
        (fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/no-titlebar.patch";
          sha256 = "sha256-NI2Xpy/BJHk3dqZgGchA1FO/4shbybQcVl4rbGEg2i8=";
        })
      ];
    })
    else emacsPgtkGcc;
in
callPackage ./wrapper.nix {
  inherit emacs;
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
      nixpkgs-fmt
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

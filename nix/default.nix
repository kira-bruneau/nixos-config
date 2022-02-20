{ lib
, pkgs
, stdenv
, fetchpatch
, emacsGcc
, emacsPgtkGcc
, callPackage
, writeShellScriptBin
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

  nodePackages = import ./node-packages/node-composition.nix {
    inherit pkgs;
    inherit (stdenv.hostPlatform) system;
  };

  # A lightweight wrapper for prettierd to avoid the overhead of node
  prettierc = (callPackage ./core_d_client.nix {
    name = "prettierc";
    serverPath = ''${nodePackages."@fsouza/prettierd"}/bin/prettierd'';
    configFile = ".prettierd";
  });
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
      nodePackages."@fsouza/prettierd"
      nodePackages.typescript
      nodePackages.typescript-language-server
      pandoc
      perl
      prettierc
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

{ lib
, pkgs
, stdenv
, fetchpatch
, emacs29
, emacs29-pgtk
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
, eslint_d
, fd
, fzf
, gcc
, gdb
, ggt
, git
, go
, godef
, gopls
, imagemagick
, jdk
, libnotify
, lldb
, nixd
, nixpkgs-fmt
, nodejs
, nodePackages
, omnisharp-roslyn
, ollama
, pandoc
, perl
, prettierd
, python3
, ripgrep
, rust-analyzer
, rustc
, rustfmt
, solargraph
, tectonic
, texlab
, yarn
, vala-language-server
}:

let
  emacs =
    if stdenv.hostPlatform.isDarwin
    then emacs29
    else emacs29-pgtk;

  # A lightweight wrapper for prettierd to avoid the overhead of node
  prettierc = (callPackage ./core_d_client.nix {
    name = "prettierc";
    serverPath = ''${prettierd}/bin/prettierd'';
    configFile = ".prettierd";
  });

  # A lightweight wrapper for eslint_d to avoid the overhead of node
  eslint_c = (callPackage ./core_d_client.nix {
    name = "eslint_c";
    serverPath = ''${eslint_d}/bin/eslint_d'';
    configFile = ".eslint_d";
  });
in
callPackage ./wrapper.nix {
  emacs = emacs.pkgs.withPackages (epkgs: [
    epkgs.adaptive-wrap
    epkgs.amx
    epkgs.apheleia
    epkgs.arduino-mode
    epkgs.async
    epkgs.avy
    epkgs.beacon
    epkgs.browse-at-remote
    epkgs.buffer-move
    epkgs.ccls
    epkgs.cider
    epkgs.cmake-font-lock
    epkgs.company
    epkgs.company-flx
    epkgs.company-restclient
    epkgs.counsel
    epkgs.counsel-projectile
    epkgs.dap-mode
    epkgs.diminish
    epkgs.direnv
    epkgs.doom-themes
    epkgs.drag-stuff
    epkgs.dtrt-indent
    epkgs.editorconfig
    epkgs.ellama
    epkgs.evil
    epkgs.evil-collection
    epkgs.flx
    epkgs.flycheck
    epkgs.forge
    epkgs.gcmh
    epkgs.git-modes
    epkgs.graphql-ts-mode
    epkgs.ivy
    epkgs.latex-preview-pane
    epkgs.lsp-java
    epkgs.lsp-mode
    epkgs.lsp-ui
    epkgs.lua-mode
    epkgs.macrostep
    epkgs.magit
    epkgs.markdown-mode
    epkgs.mermaid-mode
    epkgs.multi-term
    epkgs.multiple-cursors
    epkgs.nameless
    epkgs.nix-ts-mode
    epkgs.org-download
    epkgs.page-break-lines
    epkgs.php-mode
    epkgs.pkgbuild-mode
    epkgs.powerline
    epkgs.powershell
    epkgs.presentation
    epkgs.projectile
    epkgs.rainbow-delimiters
    epkgs.restclient
    epkgs.rg
    epkgs.slime
    epkgs.smartparens
    epkgs.suggest
    epkgs.swiper
    (epkgs.treesit-grammars.with-grammars (ts: [
      ts.tree-sitter-bash
      ts.tree-sitter-c
      ts.tree-sitter-c-sharp
      ts.tree-sitter-cmake
      ts.tree-sitter-cpp
      ts.tree-sitter-css
      ts.tree-sitter-dockerfile
      ts.tree-sitter-go
      ts.tree-sitter-gomod
      ts.tree-sitter-gowork
      ts.tree-sitter-graphql
      ts.tree-sitter-html
      ts.tree-sitter-java
      ts.tree-sitter-javascript
      ts.tree-sitter-jsdoc
      ts.tree-sitter-json
      ts.tree-sitter-nix
      ts.tree-sitter-python
      ts.tree-sitter-ruby
      ts.tree-sitter-rust
      ts.tree-sitter-typescript
      ts.tree-sitter-yaml
    ]))
    epkgs.undo-tree
    epkgs.vala-mode
    epkgs.visual-regexp
    epkgs.visual-regexp-steroids
    epkgs.vlf
    epkgs.web-mode
    epkgs.which-key
    epkgs.whitespace-cleanup-mode
  ]);

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
      eslint_c
      eslint_d
      fd
      fzf
      gcc
      gdb
      ggt
      git
      go
      godef
      gopls
      imagemagick
      jdk
      libnotify
      nixd
      nixpkgs-fmt
      nodejs
      nodePackages.bash-language-server
      nodePackages.typescript
      nodePackages.typescript-language-server
      (ollama.override { enableRocm = true; })
      pandoc
      perl
      prettierc
      prettierd
      (python3.withPackages (ps: with ps; [
        debugpy
        python-lsp-server
      ]))
      ripgrep
      rust-analyzer
      rustc
      rustfmt
      solargraph
      tectonic
      texlab
      yarn
      vala-language-server
    ] ++ lib.optionals (!stdenv.hostPlatform.isDarwin) [
      # Currently doesn't built on Darwin
      ccls
      (lib.lowPrio lldb) # collides with six.py required by python-lsp-server
      omnisharp-roslyn
    ];
  };

  config = ./config;
}

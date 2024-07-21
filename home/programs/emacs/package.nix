{
  lib,
  stdenv,
  emacs29,
  emacs29-pgtk,
  callPackage,
  buildEnv,
  aspellWithDicts,
  bear,
  cargo,
  cargo-edit,
  ccls,
  cmake,
  cmake-language-server,
  coreutils,
  diffutils,
  direnv,
  dockerfile-language-server-nodejs,
  emacs-lsp-booster,
  eslint_d,
  fd,
  fzf,
  gcc,
  gdb,
  ggt,
  git,
  go,
  godef,
  gopls,
  imagemagick,
  jdk,
  libnotify,
  marksman,
  lldb,
  nixd,
  nixfmt-rfc-style,
  nodejs,
  nodePackages,
  omnisharp-roslyn,
  pandoc,
  perl,
  prettierd,
  python3,
  ripgrep,
  rust-analyzer,
  rustc,
  rustfmt,
  solargraph,
  texlab,
  texliveBasic,
  vala-language-server,
  vscode-langservers-extracted,
  yarn,
}:

let
  emacs = if stdenv.hostPlatform.isDarwin then emacs29 else emacs29-pgtk;

  emacsPackages = emacs.pkgs.overrideScope (
    final: prev: {
      lsp-mode = prev.lsp-mode.overrideAttrs (attrs: {
        LSP_USE_PLISTS = true;
      });
    }
  );
in
callPackage ./wrapper.nix {
  emacs = emacsPackages.emacsWithPackages (epkgs: [
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
    epkgs.evil-textobj-tree-sitter
    epkgs.flx
    epkgs.flycheck
    epkgs.forge
    epkgs.gcmh
    epkgs.git-modes
    epkgs.graphql-ts-mode
    epkgs.haskell-mode
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
    epkgs.pdf-tools
    epkgs.php-mode
    epkgs.pkgbuild-mode
    epkgs.powerline
    epkgs.powershell
    epkgs.presentation
    epkgs.pretty-sha-path
    epkgs.projection
    epkgs.projection-multi
    epkgs.rainbow-delimiters
    epkgs.restclient
    epkgs.rg
    epkgs.slime
    epkgs.smartparens
    epkgs.suggest
    epkgs.swiper
    epkgs.tree-sitter-ispell
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
      ts.tree-sitter-graphql
      ts.tree-sitter-java
      ts.tree-sitter-javascript
      ts.tree-sitter-json
      ts.tree-sitter-nix
      ts.tree-sitter-python
      ts.tree-sitter-ruby
      ts.tree-sitter-rust
      ts.tree-sitter-toml
      ts.tree-sitter-tsx
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
    paths =
      [
        (aspellWithDicts (
          dicts: with dicts; [
            en
            en-computers
            en-science
          ]
        ))
        bear
        cargo
        cargo-edit
        cmake
        cmake-language-server
        coreutils
        diffutils
        direnv
        dockerfile-language-server-nodejs
        emacs-lsp-booster
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
        marksman
        nixd
        nixfmt-rfc-style
        nodejs
        nodePackages.bash-language-server
        nodePackages.typescript
        nodePackages.typescript-language-server
        nodePackages.vue-language-server
        nodePackages.yaml-language-server
        pandoc
        perl
        prettierd
        (python3.withPackages (
          ps: with ps; [
            debugpy
            python-lsp-server
          ]
        ))
        ripgrep
        rust-analyzer
        rustc
        rustfmt
        solargraph
        texlab
        (texliveBasic.withPackages (
          ps: with ps; [
            arydshln
            ec
            fontawesome5
            metafont
            moderncv
            multirow
            pgf
            ps.import
          ]
        ))
        vala-language-server
        vscode-langservers-extracted
        yarn
      ]
      ++ lib.optionals (!stdenv.hostPlatform.isDarwin) [
        # Currently doesn't built on Darwin
        ccls
        (lib.lowPrio lldb) # collides with six.py required by python-lsp-server
        omnisharp-roslyn
      ];
  };

  config = ./config;
}

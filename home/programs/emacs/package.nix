{
  lib,
  pkgsUnstable,
  stdenv,
  emacs29,
  emacs29-pgtk,
  callPackage,
  fetchFromGitHub,
  buildEnv,
  aspellWithDicts,
  bear,
  cargo,
  cargo-edit,
  clang-tools,
  cmake,
  cmake-language-server,
  coreutils,
  dejavu_fonts,
  diffutils,
  direnv,
  dockerfile-language-server-nodejs,
  emacs-all-the-icons-fonts,
  eslint_d,
  fd,
  fzf,
  gcc,
  gdb,
  git,
  go,
  godef,
  gopls,
  imagemagick,
  jdk,
  jdt-language-server,
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
in
callPackage ./wrapper.nix {
  emacs = emacs.pkgs.emacsWithPackages (epkgs: [
    epkgs.all-the-icons-dired
    epkgs.acm-terminal
    epkgs.adaptive-wrap
    epkgs.apheleia
    epkgs.arduino-mode
    epkgs.async
    epkgs.avy
    epkgs.browse-at-remote
    epkgs.buffer-move
    epkgs.cmake-font-lock
    epkgs.cmake-mode
    epkgs.consult
    epkgs.difftastic
    epkgs.direnv
    epkgs.doom-themes
    epkgs.drag-stuff
    epkgs.dtrt-indent
    epkgs.editorconfig
    epkgs.ellama
    epkgs.embark
    epkgs.embark-consult
    epkgs.evil
    epkgs.evil-collection
    epkgs.evil-mc
    epkgs.evil-textobj-tree-sitter
    epkgs.flx
    epkgs.flycheck
    epkgs.forge
    epkgs.gcmh
    epkgs.git-modes
    epkgs.graphql-ts-mode
    epkgs.haskell-mode
    epkgs.latex-preview-pane
    epkgs.lsp-bridge
    epkgs.lua-mode
    epkgs.macrostep
    epkgs.magit
    epkgs.marginalia
    epkgs.markdown-mode
    epkgs.mermaid-mode
    epkgs.nameless
    (epkgs.nix-ts-mode.overrideAttrs (
      finalAttrs: prevAttrs:
      assert prevAttrs.version == "20231107.1639";
      {
        version = "0.1.4";
        src = fetchFromGitHub {
          owner = "nix-community";
          repo = "nix-ts-mode";
          rev = "refs/tags/v${finalAttrs.version}";
          hash = "sha256-hVJyVoTrHx3BJ5Te1ovEFgAqmALe4vX2oKyMl1FlU+g=";
        };
      }
    ))
    epkgs.orderless
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
    epkgs.projection-multi-embark
    epkgs.rainbow-delimiters
    epkgs.restclient
    epkgs.smartparens
    epkgs.sudo-edit
    epkgs.suggest
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
    epkgs.vertico
    epkgs.visual-regexp
    epkgs.visual-regexp-steroids
    epkgs.vlf
    epkgs.web-mode
    epkgs.which-key
    epkgs.whitespace-cleanup-mode
    epkgs.xterm-color
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
        clang-tools
        cmake
        cmake-language-server
        coreutils
        dejavu_fonts
        diffutils
        direnv
        dockerfile-language-server-nodejs
        emacs-all-the-icons-fonts
        eslint_d
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
        jdt-language-server
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
        pkgsUnstable.rust-analyzer
        rustc
        rustfmt
        solargraph
        texlab
        texliveBasic
        vala-language-server
        vscode-langservers-extracted
        yarn
      ]
      ++ lib.optionals (!stdenv.hostPlatform.isDarwin) [
        # Currently doesn't built on Darwin
        (lib.lowPrio lldb) # collides with six.py required by python-lsp-server
        omnisharp-roslyn
      ];
  };

  config = ./config;
}

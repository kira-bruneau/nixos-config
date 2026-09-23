{
  lib,
  emacs-pgtk,
  callPackage,
  fetchFromCodeberg,
  tree-sitter,
  fetchFromGitHub,
  buildEnv,

  coreutils,
  diffutils,
  fd-relative-full-path,
  git,
  libnotify,
  nixd,
  nixfmt,
  ripgrep,

  minimal ? false,
  aspellWithDicts,
  bash-language-server,
  bear,
  cargo,
  cargo-edit,
  clang-tools,
  cmake,
  cmake-language-server,
  direnv,
  dockerfile-language-server,
  emacs-all-the-icons-fonts,
  eslint_d,
  gcc,
  gdb,
  go,
  godef,
  gopls,
  imagemagick,
  jdk,
  jdt-language-server,
  kotlin-language-server,
  lemminx,
  marksman,
  lldb,
  nodejs,
  omnisharp-roslyn,
  pandoc,
  perl,
  phpactor,
  prettierd,
  python3,
  rust-analyzer,
  rustc,
  rustfmt,
  solargraph,
  texlab,
  texliveBasic,
  typescript,
  typescript-language-server,
  vala-language-server,
  vscode-langservers-extracted,
  wgsl-analyzer,
  yaml-language-server,
  yarn,
}:

let
  emacs = emacs-pgtk;
in
callPackage ./wrapper.nix {
  emacs = emacs.pkgs.emacsWithPackages (
    epkgs:
    [
      epkgs.adaptive-wrap
      epkgs.apheleia
      epkgs.async
      epkgs.avy
      epkgs.buffer-move
      (
        assert lib.versionOlder emacs.version "31";
        epkgs.ben.overrideAttrs (attrs: {
          src = fetchFromCodeberg {
            owner = "pastor";
            repo = "ben.el";
            rev = "v0.12.13";
            hash = "sha256-hgLmP0e0Gr0y6jLBWyHFf654fBeQqoP9ZVyJAWgQ+uc=";
          };
        })
      )
      epkgs.consult
      epkgs.doom-themes
      epkgs.drag-stuff
      epkgs.dtrt-indent
      epkgs.embark
      epkgs.embark-consult
      epkgs.evil
      epkgs.evil-collection
      epkgs.evil-mc
      epkgs.evil-textobj-tree-sitter
      epkgs.gcmh
      epkgs.git-modes
      epkgs.magit
      epkgs.marginalia
      epkgs.nameless
      epkgs.nix-ts-mode
      epkgs.orderless
      epkgs.page-break-lines
      epkgs.powerline
      epkgs.pretty-sha-path
      epkgs.projection
      epkgs.projection-multi
      epkgs.projection-multi-embark
      epkgs.rainbow-delimiters
      epkgs.smartparens
      epkgs.sudo-edit
      epkgs.tree-sitter-ispell
      (epkgs.treesit-grammars.with-grammars (
        ts:
        [
          ts.tree-sitter-bash
          ts.tree-sitter-c
          ts.tree-sitter-c-sharp
          ts.tree-sitter-cmake
          ts.tree-sitter-cpp
          ts.tree-sitter-css
          ts.tree-sitter-dockerfile
          ts.tree-sitter-go
          ts.tree-sitter-gomod
          ts.tree-sitter-html
          ts.tree-sitter-java
          ts.tree-sitter-javascript
          ts.tree-sitter-jsdoc
          ts.tree-sitter-json
          ts.tree-sitter-lua
          ts.tree-sitter-nix
          (
            assert lib.versionOlder emacs.version "31";
            tree-sitter.buildGrammar {
              language = "php";
              version = "0.23.12";
              src = fetchFromGitHub {
                owner = "tree-sitter";
                repo = "tree-sitter-php";
                rev = "v0.23.12";
                hash = "sha256-UWYKrC0mpWO86V52Phj/gYCdH586ZNdev/zhvUn4EBc=";
              };
              location = "php";
            }
          )
          ts.tree-sitter-phpdoc
          ts.tree-sitter-python
          ts.tree-sitter-ruby
          ts.tree-sitter-rust
          ts.tree-sitter-tsx
          ts.tree-sitter-typescript
          ts.tree-sitter-wgsl
          ts.tree-sitter-yaml
        ]
        ++ lib.optionals (!minimal) [
          ts.tree-sitter-kotlin
          ts.tree-sitter-graphql
        ]
      ))
      epkgs.undo-tree
      epkgs.vertico
      epkgs.visual-regexp
      epkgs.visual-regexp-steroids
      epkgs.vlf
      epkgs.wgrep
      epkgs.whitespace-cleanup-mode
      epkgs.xterm-color
    ]
    ++ lib.optionals (!minimal) [
      epkgs.acm-terminal
      epkgs.all-the-icons-dired
      epkgs.arduino-mode
      epkgs.browse-at-remote
      epkgs.cmake-font-lock
      epkgs.cmake-mode
      epkgs.fish-mode
      epkgs.flycheck
      epkgs.forge
      epkgs.graphql-ts-mode
      epkgs.haskell-mode
      epkgs.journalctl-mode
      epkgs.kotlin-ts-mode
      epkgs.latex-preview-pane
      epkgs.lsp-bridge
      epkgs.macrostep
      epkgs.markdown-mode
      epkgs.org-download
      epkgs.pdf-tools
      epkgs.pkgbuild-mode
      epkgs.powershell
      epkgs.presentation
      epkgs.restclient
      epkgs.vala-mode
      epkgs.web-mode
    ]
  );

  profile = buildEnv {
    name = "emacs-profile";
    paths = [
      coreutils
      diffutils
      fd-relative-full-path
      git
      libnotify
      nixfmt
      ripgrep
    ]
    ++ lib.optionals (!minimal) [
      (aspellWithDicts (
        dicts: with dicts; [
          en
          en-computers
          en-science
        ]
      ))
      bash-language-server
      bear
      cargo
      cargo-edit
      clang-tools
      cmake
      cmake-language-server
      direnv
      dockerfile-language-server
      emacs-all-the-icons-fonts
      eslint_d
      gcc
      gdb
      go
      godef
      gopls
      imagemagick
      jdk
      jdt-language-server
      kotlin-language-server
      lemminx
      lldb
      marksman
      nixd
      nodejs
      omnisharp-roslyn
      pandoc
      perl
      phpactor
      prettierd
      (python3.withPackages (
        ps: with ps; [
          debugpy
          python-lsp-server
        ]
      ))
      rust-analyzer
      rustc
      rustfmt
      solargraph
      texlab
      texliveBasic
      typescript
      typescript-language-server
      vala-language-server
      vscode-langservers-extracted
      wgsl-analyzer
      yaml-language-server
      yarn
    ];
  };

  config = ./config;
}

{ config, pkgs, ... }:

let
  emacs = config.programs.emacs.package;

  # Create emacseditor script and desktop file for emacsclient
  # Taken from nixpkgs/nixos/modules/services/editors/emacs.nix
  editorScript = pkgs.writeScriptBin "emacseditor" ''
    #!${pkgs.runtimeShell}
    if [ -z "$1" ]; then
      exec ${emacs}/bin/emacsclient --create-frame --alternate-editor ${emacs}/bin/emacs
    else
      exec ${emacs}/bin/emacsclient --alternate-editor ${emacs}/bin/emacs "$@"
    fi
  '';

  desktopApplicationFile = pkgs.writeTextFile {
    name = "emacsclient.desktop";
    destination = "/share/applications/emacsclient.desktop";
    text = ''
      [Desktop Entry]
      Name=Emacsclient
      GenericName=Text Editor
      Comment=Edit text
      MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
      Exec=emacseditor %F
      Icon=emacs
      Type=Application
      Terminal=false
      Categories=Development;TextEditor;
      StartupWMClass=Emacs
      Keywords=Text;Editor;
    '';
  };
in {
  programs.emacs.enable = true;
  services.emacs.enable = true;

  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    ccls
    desktopApplicationFile
    diffutils
    editorScript
    fd
    fzf
    gdb
    git
    jdk
    nodejs
    nodePackages.bash-language-server
    nodePackages.typescript
    nodePackages.typescript-language-server
    pandoc
    python37Packages.pycodestyle
    python37Packages.pyflakes
    python37Packages.python-language-server
    python37Packages.rope
    python37Packages.yapf
    ripgrep
    rnix-lsp
    rust-analyzer
    rustc
    texlab
  ];

  home.sessionVariables = {
    # Set rust source path for rust-analyzer
    RUST_SRC_PATH = pkgs.rustPackages.rustPlatform.rustcSrc;
  };
}

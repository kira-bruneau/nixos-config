{ profile, makeWrapperArgs ? [],
  writeScriptBin, runtimeShell, writeTextFile,
  runCommand, lib, emacs, makeWrapper
}:

let
  # Create emacseditor script and desktop file for emacsclient
  # Taken from nixpkgs/nixos/modules/services/editors/emacs.nix
  editorScript = writeScriptBin "emacseditor" ''
    #!${runtimeShell}
    if [ -z "$1" ]; then
      exec emacsclient --create-frame --alternate-editor ${emacs}/bin/emacs
    else
      exec emacsclient --alternate-editor ${emacs}/bin/emacs "$@"
    fi
  '';

  desktopApplicationFile = writeTextFile {
    name = "emacsclient.desktop";
    destination = "/share/applications/emacsclient.desktop";
    text = ''
      [Desktop Entry]
      Name=Emacs (Client)
      GenericName=Text Editor
      Comment=Edit text
      MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
      Exec=emacseditor %F
      Icon=emacs
      Type=Application
      Terminal=false
      Categories=Development;TextEditor;
      StartupWMClass=Emacsd
      Keywords=Text;Editor;
    '';
  };
in runCommand
  "${emacs.name}-profile-wrapper"
  {
    nativeBuildInputs = [ makeWrapper ];
  }
  ''
    mkdir -p "$out"
    ln -s "${emacs}/"* "$out"

    rm "$out/bin"
    mkdir "$out/bin"
    ln -s "${emacs}/bin/"* "$out/bin"

    for bin in "${emacs.name}" emacsclient; do
      rm "$out/bin/$bin"
      makeWrapper "${emacs}/bin/$bin" "$out/bin/$bin" \
        --prefix NIX_PROFILES ' ' "${profile}" \
        --prefix PATH : "${profile}/bin" \
        ${lib.concatStringsSep " " makeWrapperArgs}
    done

    rm "$out/bin/emacs"
    ln -s "$out/bin/${emacs.name}" "$out/bin/emacs"
    cp "${editorScript}/bin/emacseditor" "$out/bin"

    rm "$out/share"
    mkdir "$out/share"
    ln -s "${emacs}/share/"* "$out/share"

    rm "$out/share/applications"
    mkdir "$out/share/applications"
    ln -s "${emacs}/share/applications/"* "$out/share/applications"
    cp -f "${desktopApplicationFile}/share/applications/emacsclient.desktop" "$out/share/applications"
  ''

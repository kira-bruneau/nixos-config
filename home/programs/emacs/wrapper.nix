{
  lib,
  writeScriptBin,
  runtimeShell,
  writeTextFile,
  coreutils,
  makeFontsConf,
  runCommand,
  makeWrapper,
  emacs,
  profile,
  config,
}:

let
  # Create emacseditor script and desktop file for emacsclient
  # Taken from nixpkgs/nixos/modules/services/editors/emacs.nix
  editorScript = writeScriptBin "emacseditor" ''
    #!${runtimeShell}
    emacs="$(dirname "''${BASH_SOURCE[0]}")/emacs"
    if [ -z "$1" ]; then
      exec emacsclient --create-frame --alternate-editor "$emacs"
    else
      exec emacsclient --alternate-editor "$emacs" "$@"
    fi
  '';

  # Export environment variables defined in interactive & login
  # shells. Needed for macOS Emacs.app package.
  shellEnv = ''
    if [ -n "$SHELL" ]; then
      while IFS='=' read -rd $'\0' name value; do
        export "$name"="$value"
      done < <("$SHELL" -ilc '${coreutils}/bin/env -0')
    fi
  '';

  makeWrapperArgs = [
    "--prefix NIX_PROFILES ' ' ${profile}"
    "--prefix PATH : ${profile}/bin"
    "--set FONTCONFIG_FILE ${makeFontsConf { fontDirectories = [ "${profile}/share/fonts" ]; }}"
    "--add-flags '--init-directory=${config}'"
  ];
in
runCommand "${emacs.name}-profile-wrapper"
  {
    nativeBuildInputs = [ makeWrapper ];
    meta.mainProgram = "emacs";
  }
  ''
    mkdir "$out"
    ln -s ${emacs}/* "$out"

    rm "$out"/bin; mkdir "$out"/bin
    ln -s ${emacs}/bin/* "$out"/bin

    for bin in $(basename "$out"/bin/emacs-*); do
      rm "$out"/bin/"$bin"
      makeWrapper ${emacs}/bin/"$bin" "$out"/bin/"$bin" \
        ${lib.concatStringsSep " " makeWrapperArgs}
    done

    ln -sf "$out"/bin/emacs-* "$out"/bin/emacs
    cp ${editorScript}/bin/emacseditor "$out"/bin

    rm "$out"/share; mkdir "$out"/share
    ln -s ${emacs}/share/* "$out"/share

    rm "$out"/share/applications; mkdir "$out"/share/applications
    ln -s ${emacs}/share/applications/* "$out"/share/applications
    rm "$out"/share/applications/emacsclient.desktop
    sed 's/Exec=.*/Exec=emacseditor %F/' \
      ${emacs}/share/applications/emacsclient.desktop > "$out"/share/applications/emacsclient.desktop

    if [ -d ${emacs}/Applications/Emacs.app ]; then
      rm "$out"/Applications; mkdir "$out"/Applications
      ln -s ${emacs}/Applications/* "$out"/Applications
      rm "$out"/Applications/Emacs.app; mkdir -p "$out"/Applications/Emacs.app/Contents
      ln -s ${emacs}/Applications/Emacs.app/Contents/* "$out"/Applications/Emacs.app/Contents
      rm "$out"/Applications/Emacs.app/Contents/MacOS; mkdir "$out"/Applications/Emacs.app/Contents/MacOS
      ln -s ${emacs}/Applications/Emacs.app/Contents/MacOS/* "$out"/Applications/Emacs.app/Contents/MacOS

      rm "$out"/Applications/Emacs.app/Contents/MacOS/Emacs
      makeWrapper ${emacs}/Applications/Emacs.app/Contents/MacOS/Emacs "$out"/Applications/Emacs.app/Contents/MacOS/Emacs \
        --run ${lib.escapeShellArg shellEnv} \
        ${lib.optionalString (lib.hasInfix "emacs-mac" emacs.name) "--set EMACS_REINVOKED_FROM_SHELL 1"} \
        ${lib.concatStringsSep " " makeWrapperArgs}
    fi
  ''

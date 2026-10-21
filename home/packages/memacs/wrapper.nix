{
  writeScript,
  runtimeShell,
  makeFontsConf,
  emacs,
  profile,
  config,
}:

let
  # Editor script derived from emacsclient.desktop
  editorScript = writeScript "emacseditor" ''
    #!${runtimeShell}
    if [ $# -eq 0 ]; then
      exec emacsclient --alternate-editor= --create-frame
    else
      exec emacsclient --alternate-editor= --reuse-frame "$@"
    fi
  '';

  makeWrapperArgs = [
    "--prefix NIX_PROFILES ' ' ${profile}"
    "--prefix PATH : ${profile}/bin"
    "--set FONTCONFIG_FILE ${makeFontsConf { fontDirectories = [ "${profile}/share/fonts" ]; }}"
    "--add-flags '--init-directory=${config}'"
  ];
in
emacs.overrideAttrs (attrs: {
  buildCommand = ''
    eval "$(declare -f makeBinaryWrapper | sed 1s/^/_/)"
    function makeBinaryWrapper() {
      local name=$(basename "$2")
      if [[ "$name" == "emacs" || "$name" == emacs-* ]]; then
        _makeBinaryWrapper "$@" ${builtins.concatStringsSep " " makeWrapperArgs}
      else
        _makeBinaryWrapper "$@"
      fi
    }
    ${attrs.buildCommand}
    cp ${editorScript} "$out/bin/emacseditor"
    rm "$out/share/applications"; mkdir "$out/share/applications"
    ln -s "$emacs/share/applications/"* "$out/share/applications"
    rm "$out/share/applications/emacsclient.desktop"
    sed 's/Exec=.*/Exec=emacseditor %F/' \
      ${emacs}/share/applications/emacsclient.desktop > "$out/share/applications/emacsclient.desktop"
  '';
})

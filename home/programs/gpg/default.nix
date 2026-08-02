{ config, pkgs, ... }:

{
  programs.gpg = {
    enable = true;
    homedir = "${config.xdg.dataHome}/gnupg";
  };

  services.gpg-agent = {
    enable = true;

    enableSshSupport = true;
    defaultCacheTtl = 240;
    defaultCacheTtlSsh = 240;

    # Workaround for slow gnome pinentry
    # Source: https://dev.gnupg.org/T3240
    noAllowExternalCache = true;

    pinentry.package = pkgs.writeShellScriptBin "pinentry" ''
      if [ -n "$DISPLAY" ]; then
        exec ${pkgs.pinentry-gnome3}/bin/pinentry-gnome3 "$@"
      fi

      exec ${pkgs.pinentry-tty}/bin/pinentry-tty "$@"
    '';
  };

  programs.ssh = {
    enable = true;
    settings."*".Match = ''
      host * exec "${config.programs.gpg.package}/bin/gpg-connect-agent updatestartuptty /bye"
    '';
  };
}

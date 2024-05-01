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

    pinentryFlavor = null;
    extraConfig = ''
      pinentry-program ${pkgs.writeShellScript "pinentry" ''
        if [ -n "$DISPLAY" ]; then
          exec ${pkgs.pinentry.gnome3}/bin/pinentry-gnome3 "$@"
        fi

        exec ${pkgs.pinentry.tty}/bin/pinentry-tty "$@"
      ''}

      # Workaround for slow gnome pinentry
      # Source: https://dev.gnupg.org/T3240
      no-allow-external-cache
    '';
  };

  programs.ssh = {
    enable = true;
    matchBlocks."*".match = ''
      host * exec "${config.programs.gpg.package}/bin/gpg-connect-agent updatestartuptty /bye"
    '';
  };
}

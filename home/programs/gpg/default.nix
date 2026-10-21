{ config, pkgsCustom, ... }:

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

    pinentry.package = pkgsCustom.pinentry;
  };

  programs.ssh = {
    enable = true;
    settings."*".Match = ''
      host * exec "${config.programs.gpg.package}/bin/gpg-connect-agent updatestartuptty /bye"
    '';
  };
}

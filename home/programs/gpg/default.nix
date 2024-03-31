{ config, ... }:

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
  };

  programs.ssh = {
    enable = true;
    matchBlocks."*".match = ''
      host * exec "${config.programs.gpg.package}/bin/gpg-connect-agent updatestartuptty /bye"
    '';
  };
}

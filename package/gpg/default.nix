{ config, lib, ... }:

{
  programs.gpg = {
    enable = true;
    homedir = "${config.xdg.dataHome}/gnupg";
    publicKeys = [
      {
        # TODO: Refactor home-manager module so it doesn't put the source in the global nix store
        source = config.lib.file.mkOutOfStoreSymlink
          "${config.home.homeDirectory}/Auth/kira.asc";
      }
    ];
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 240;
    defaultCacheTtlSsh = 240;
    pinentryFlavor = "gnome3";
  };
}

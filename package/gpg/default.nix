{ ... }:

{
  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 240;
    defaultCacheTtlSsh = 240;
    pinentryFlavor = "gnome3";
  };
}

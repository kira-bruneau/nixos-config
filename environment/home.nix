{ config, pkgs, ... }:

{
  # Locale & Timezone
  i18n.defaultLocale = "en_CA.UTF-8";
  time.timeZone = "America/Toronto";
  services.localtime.enable = true;
}

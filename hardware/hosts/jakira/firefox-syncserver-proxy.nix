{
  services.nginx.virtualHosts = {
    "firefox-syncserver.jakira.space" = {
      enableACME = true;
      forceSSL = true;
      locations."/".proxyPass = "http://quartz:5000";
    };
  };
}

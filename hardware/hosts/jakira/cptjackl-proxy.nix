{
  services.nginx.virtualHosts = {
    "cptjackl.quest" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://spinel:30000";
        recommendedProxySettings = true;
        proxyWebsockets = true;
      };
    };
    "www.cptjackl.quest" = {
      enableACME = true;
      forceSSL = true;
      locations."/".return = "301 $scheme://cptjackl.quest$request_uri";
    };
  };
}

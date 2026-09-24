{
  services.nginx.virtualHosts = {
    "cptjackl.com" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://jasper:5225";
        recommendedProxySettings = true;
        proxyWebsockets = true;
      };
    };
    "foundry.cptjackl.com" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://jasper:30000";
        recommendedProxySettings = true;
        proxyWebsockets = true;
      };
    };
    "lore.cptjackl.com" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://jasper:30001";
        recommendedProxySettings = true;
        proxyWebsockets = true;
      };
    };
    "mine.cptjackl.com" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://jasper:25565";
        recommendedProxySettings = true;
      };
    };
  };
}

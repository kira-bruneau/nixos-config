{
  services.nginx.virtualHosts = {
    "cptjackl.quest" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://jasper:5225";
        recommendedProxySettings = true;
        proxyWebsockets = true;
      };
    };
    "foundry.cptjackl.quest" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://jasper:30000";
        recommendedProxySettings = true;
        proxyWebsockets = true;
      };
    };
    "lore.cptjackl.quest" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://jasper:30001";
        recommendedProxySettings = true;
        proxyWebsockets = true;
      };
    };
    "mine.cptjackl.quest" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://jasper:25565";
        recommendedProxySettings = true;
      };
    };
  };
}

{ pkgs, pkgsUnstable, ... }:

let
  clientConfig."m.homeserver".base_url = "https://matrix.jakira.space";
  serverConfig."m.server" = "matrix.jakira.space:443";
  mkWellKnown = data: ''
    default_type application/json;
    add_header Access-Control-Allow-Origin *;
    return 200 '${builtins.toJSON data}';
  '';
in
{
  services.nginx = {
    enable = true;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;
    virtualHosts = {
      "jakira.space" = {
        enableACME = true;
        forceSSL = true;
        locations."= /.well-known/matrix/server".extraConfig = mkWellKnown serverConfig;
        locations."= /.well-known/matrix/client".extraConfig = mkWellKnown clientConfig;
      };
      "matrix.jakira.space" = {
        enableACME = true;
        forceSSL = true;
        locations = {
          "/".return = 404;
          "/_matrix" = {
            proxyPass = "http://quartz:8008";
            extraConfig = ''
              client_max_body_size 1G;
            '';
          };
          "/_synapse/client".proxyPass = "http://quartz:8008";
        };
      };
      "chat.jakira.space" = {
        enableACME = true;
        forceSSL = true;
        root = pkgsUnstable.cinny.override {
          conf = {
            defaultHomeserver = 0;
            homeserverList = [ "jakira.space" ];
            hashRouter = {
              enabled = true;
              basename = "/";
            };
          };
        };
      };
      "element.jakira.space" = {
        enableACME = true;
        forceSSL = true;
        root = pkgs.element-web.override {
          conf = {
            default_server_config = clientConfig;
            default_server_name = "jakira.space";
            default_theme = "dark";
          };
        };
      };
    };
  };
}

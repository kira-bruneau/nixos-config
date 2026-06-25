{ pkgs, ... }:

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
  services.nginx.virtualHosts = {
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
      root = pkgs.cinny.override {
        conf = {
          defaultHomeserver = 0;
          homeserverList = [ "jakira.space" ];
        };
      };

      extraConfig = ''
        rewrite ^/config.json$ /config.json break;
        rewrite ^/manifest.json$ /manifest.json break;

        rewrite ^/sw.js$ /sw.js break;
        rewrite ^/pdf.worker.min.js$ /pdf.worker.min.js break;

        rewrite ^/public/(.*)$ /public/$1 break;
        rewrite ^/assets/(.*)$ /assets/$1 break;

        rewrite ^(.+)$ /index.html break;
      '';
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
}

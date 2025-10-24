{
  services.nginx = {
    enable = true;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;
    commonHttpConfig = ''
      log_format systemd '$remote_addr - $remote_user '
                         '"$request" $status $body_bytes_sent '
                         '"$http_referer" "$http_user_agent"';

      access_log syslog:server=unix:/dev/log systemd;
      error_log syslog:server=unix:/dev/log;
    '';
  };
}

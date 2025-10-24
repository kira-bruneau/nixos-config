{
  config,
  lib,
  ...
}:

{
  imports = [
    ../environments/default.nix
    ../services/nginx.nix
    ../users/kira.nix
  ];

  system.stateVersion = "24.11";

  users.defaultUser = "kira";

  security.acme = {
    acceptTerms = true;
    defaults.email = "kira.bruneau@pm.me";
  };

  services.mastodon = {
    enable = true;
    localDomain = "mastodon.jakira.space";
    configureNginx = true;
    smtp.fromAddress = "noreply@jakira.space";
    streamingProcesses = 1;
  };

  # https://ricard.dev/improving-mastodons-disk-usage
  systemd.services.mastodon-media-auto-remove.script =
    let
      cfg = config.services.mastodon;
      olderThanDays = toString cfg.mediaAutoRemove.olderThanDays;
    in
    lib.mkForce ''
      ${cfg.package}/bin/tootctl accounts prune
      ${cfg.package}/bin/tootctl statuses remove --days ${olderThanDays}
      ${cfg.package}/bin/tootctl media remove --days=${olderThanDays}
      ${cfg.package}/bin/tootctl media remove --remove-headers --include-follows --days 0
      ${cfg.package}/bin/tootctl preview_cards remove --days=${olderThanDays}
      ${cfg.package}/bin/tootctl media remove-orphans
    '';

  services.opendkim = {
    enable = true;
    selector = "mail";
    domains = "csl:jakira.space";
  };

  # Allow users in opendkim group to read & write
  systemd.services.opendkim.serviceConfig.UMask = lib.mkForce "0017";

  services.postfix.config = {
    milter_protocol = "6";
    smtpd_milters = [ "unix:/run/opendkim/opendkim.sock" ];
    non_smtpd_milters = [ "unix:/run/opendkim/opendkim.sock" ];
  };

  users.users.postfix.extraGroups = [ "opendkim" ];

  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  services.headscale = {
    enable = true;
    settings = {
      server_url = "https://headscale.jakira.space";
      dns.magic_dns = false;
    };
  };

  services.nginx.virtualHosts."headscale.jakira.space" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://localhost:${toString config.services.headscale.port}";
      proxyWebsockets = true;
    };
  };
}

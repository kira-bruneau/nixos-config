{
  imports = [
    ../../environment/desktop.nix
    ../../environment/gaming.nix
    ../../service/kubo.nix
    ../../user/builder.nix
    ../../user/kira.nix
  ];

  system.stateVersion = "22.11";

  containers.media-server = {
    autoStart = true;

    bindMounts = {
      "/srv" = {
        hostPath = "/srv";
        isReadOnly = false;
      };
    };

    config = {
      imports = [ ../../environment/media-server.nix ];
      system.stateVersion = "22.11";
      fonts.fontconfig.enable = false;
    };
  };

  networking.firewall.allowedTCPPorts = [
    8096
    5055
  ];
}

{ lib, pkgsKiraNur, ... }:

let
  cec-sync = pkgsKiraNur.cec-sync;
in
{
  environment.systemPackages = [ cec-sync ];

  systemd.user.services.cec-sync = {
    wantedBy = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = "${lib.getExe cec-sync} serve";
      Restart = "on-failure";
      RestartSec = 1;
      RestartSteps = 30;
      RestartMaxDelaySec = "1min";
    };
  };
}

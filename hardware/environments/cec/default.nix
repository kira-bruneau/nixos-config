{ pkgs, ... }:

let
  device = "dev-ttyACM1.device";

  cec-ctrl = pkgs.runCommandCC "cec-ctrl"
    {
      buildInputs = [ pkgs.libcec ];
    }
    ''
      mkdir -p "$out/bin"
      $CXX -std=c++2b -lcec ${./cec-ctrl.cpp} -o "$out/bin/cec-ctrl"
    '';
in
{
  environment.systemPackages = [
    cec-ctrl
  ];

  systemd.services.cec-ctrl = {
    wantedBy = [ device ];
    bindsTo = [ device ];
    after = [ device "pre-sleep.service" ];
    conflicts = [ "sleep.target" ];
    serviceConfig = {
      DynamicUser = true;
      SupplementaryGroups = [ "dialout" ];
      ExecStart = "${cec-ctrl}/bin/cec-ctrl serve";
    };
  };

  powerManagement.resumeCommands = ''
    ${cec-ctrl}/bin/cec-ctrl active
  '';

  powerManagement.powerDownCommands = ''
    ${cec-ctrl}/bin/cec-ctrl standby
  '';
}

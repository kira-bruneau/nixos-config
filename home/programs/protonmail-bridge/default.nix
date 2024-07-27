{ lib, pkgs, ... }:

let
  protonmail-bridge = pkgs.protonmail-bridge.overrideAttrs (attrs: {
    patches = [
      # Dont wipe vault on decryption errors
      (pkgs.fetchpatch {
        url = "https://patch-diff.githubusercontent.com/raw/ProtonMail/proton-bridge/pull/486.patch";
        hash = "sha256-UtwKBMlrPz191hBlFdOvImh3hwuHmXUY4wEUiplGFLQ=";
      })

      # Avoid conflicts between docker credentials key & proton bridge
      # credentials key (based on gnome-keyring being in PATH)
      #
      # TODO: Add key migration and upstream
      ./0001-Remove-SecretService-in-favour-of-SecretServiceDBus.patch
    ];
  });
in
{
  home.packages = with pkgs; [ protonmail-bridge ];

  systemd.user.services.protonmail-bridge = {
    Unit.After = [ "dbus.service" ];
    Service.ExecStart = "${lib.getExe protonmail-bridge} --noninteractive";
    Install.WantedBy = [ "graphical-session.target" ];
  };
}

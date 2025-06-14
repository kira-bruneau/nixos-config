{ lib, ... }:

{
  imports = [
    ../environments/autologin.nix
    ../environments/dev.nix
    ../environments/gui/sway.nix
    ../users/kira.nix
  ];

  system.stateVersion = "25.05";

  users.defaultUser = "kira";

  services.greetd.enable = lib.mkForce false;

  environment.loginShellInit = ''
    if [ -z "$WAYLAND_DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
      exec sway
    fi
  '';
}

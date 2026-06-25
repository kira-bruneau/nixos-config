{ pkgs, ... }:

{
  imports = [
    ../../home/environments/dev
    ../../home/environments/gui/sway.nix
    ../../home/environments/laptop.nix
    ../../home/environments/office.nix
  ];

  home = {
    stateVersion = "25.05";

    packages = with pkgs; [
      teams-for-linux
      vscodium
    ];
  };

  wayland.windowManager.sway.config = {
    startup = [ { command = "teams-for-linux"; } ];
    assigns."10" = [ { app_id = "^teams-for-linux$"; } ];
  };
}

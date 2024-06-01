{ pkgs, ... }:

{
  imports = [
    ./bluetooth.nix
    ./gui
    ./wifi.nix
  ];

  home.packages = with pkgs; [ snapshot ];

  services.swayidle = {
    timeouts = [
      {
        timeout = 600;
        command = "/run/current-system/systemd/bin/systemctl suspend-then-hibernate";
      }
    ];
  };
}

{ lib, ... }:

{
  imports = [
    ../environments/bluetooth.nix
    ../environments/dev.nix
    ../environments/gaming.nix
    ../environments/gui/sway.nix
    ../environments/media-production.nix
    ../environments/office.nix
  ];

  home.stateVersion = "22.11";

  programs.waybar.settings.mainBar.temperature = {
    hwmon-path-abs = "/sys/devices/platform/asus-ec-sensors/hwmon";
    input-filename = "temp2_input";
  };
}

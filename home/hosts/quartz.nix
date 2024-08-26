{
  imports = [
    ../environments/art.nix
    ../environments/bluetooth.nix
    ../environments/dev
    ../environments/gaming.nix
    ../environments/gui/sway.nix
    ../environments/office.nix
  ];

  home.stateVersion = "24.05";

  programs.waybar.settings.mainBar.temperature = {
    hwmon-path-abs = "/sys/devices/platform/asus-ec-sensors/hwmon";
    input-filename = "temp2_input";
  };
}

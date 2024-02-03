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

  programs.waybar.settings.mainBar.temperature = {
    hwmon-path-abs = "/sys/devices/platform/asus-ec-sensors/hwmon";
    input-filename = "temp2_input";
  };

  nixpkgs.config = {
    allowUnfreePredicate = pkg: builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
      "anytype"
      "anytype-heart"
      "clonehero"
      "clonehero-unwrapped"
      "data.zip"
      "discord"
      "sm64ex"
      "unrar"
      "vvvvvv"
    ];

    permittedInsecurePackages = [
      "electron-24.8.6"
    ];
  };

  home.stateVersion = "22.11";
}

{ lib, ... }:

{
  imports = [
    ../environment/bluetooth.nix
    ../environment/desktop.nix
    ../environment/gaming.nix
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
      "steam"
      "steam-original"
      "steam-run"
      "unrar"
      "vvvvvv"
    ];

    permittedInsecurePackages = [
      "electron-24.8.6"
    ];
  };

  home.stateVersion = "22.11";
}

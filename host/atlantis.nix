{ lib, ... }:

{
  imports = [
    ../environment/desktop.nix
    ../environment/gaming.nix
  ];

  programs.waybar.settings.mainBar.temperature = {
    hwmon-path-abs = "/sys/devices/platform/asus-ec-sensors/hwmon";
    input-filename = "temp2_input";
  };

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
    "Anytype"
    "clonehero"
    "clonehero-unwrapped"
    "data.zip"
    "discord"
    "runescape-launcher"
    "sm64ex"
    "steam"
    "steam-original"
    "steam-run"
    "unrar"
    "vvvvvv"
  ];

  home.stateVersion = "22.11";
}

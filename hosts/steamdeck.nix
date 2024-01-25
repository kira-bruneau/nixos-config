{ inputs, lib, ... }:

{
  imports = [
    ../environments/gaming.nix
    ../environments/gui/gnome.nix
    ../users/kira.nix
    inputs.jovian.nixosModules.default
  ];

  system.stateVersion = "23.11";

  jovian = {
    steam = {
      enable = true;
      autoStart = true;
      user = "kira";
      desktopSession = "gnome";
    };

    decky-loader.enable = true;
  };

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
    "clonehero"
    "clonehero-unwrapped"
    "data.zip"
    "discord"
    "sm64ex"
    "steam"
    "steam-jupiter-original"
    "steam-original"
    "steam-run"
    "steamdeck-hw-theme"
    "unrar"
    "vvvvvv"
  ];
}

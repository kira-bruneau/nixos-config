{ config, pkgs, ... }:

{
  imports = [
    ./minimal.nix
    ../feature/web-browser.nix
  ];

  environment.systemPackages = with pkgs; [
    keepassxc # TODO: Is a cli available for this? If so, move it to minimal.nix
    speedcrunch
  ];

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us,us";
  services.xserver.xkbVariant = "colemak,";
  services.xserver.xkbOptions = "grp:win_space_toggle";

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;
}

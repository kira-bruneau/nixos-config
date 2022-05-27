{ config, pkgs, ... }:

{
  imports = [
    ./desktop.nix
    ./wireless.nix
  ];

  # Enable touchpad support
  services.xserver.libinput = {
    enable = true;
    touchpad = {
      accelProfile = "flat";
      naturalScrolling = true;
    };
  };

  # Automatically control frequency of CPU to save power
  environment.systemPackages = [ pkgs.auto-cpufreq ];
  systemd.services.auto-cpufreq = {
    description = "auto-cpufreq - Automatic CPU speed & power optimizer for Linux";
    after = [ "network.target" "network-online.target" ];
    path = with pkgs; [ ncurses bash ];
    serviceConfig = {
      Type = "simple";
      User = "root";
      ExecStart = "${pkgs.auto-cpufreq}/bin/auto-cpufreq --daemon";
      Restart = "on-failure";
    };
    wantedBy = [ "multi-user.target" ];
  };

  # Automatically suspend on low power
  services.upower.enable = true;

  # Enable light for controlling backlight
  programs.light.enable = true;
}

{ lib, pkgs, ... }:

{
  virtualisation = {
    memorySize = 1024 * 4;
    qemu.options = [
      "-smp $(${pkgs.coreutils}/bin/nproc)"
      "-device virtio-vga-gl"
      "-display gtk,gl=on,grab-on-hover=on"
    ];
  };

  systemd.network.networks.eth0 = {
    matchConfig.Name = "eth0";
    networkConfig.DHCP = "yes";
  };

  environment.sessionVariables = {
    WLR_NO_HARDWARE_CURSORS = "1";
  };

  environment.etc."sway/config.d/io.conf".text = ''
    output "*" scale 2
  '';

  services.openssh.enable = lib.mkForce false;
  services.syncthing.enable = lib.mkForce false;
  services.tailscale.enable = lib.mkForce false;
}

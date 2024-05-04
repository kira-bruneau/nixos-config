{
  services.xserver.xkb = {
    layout = "us,us";
    variant = "colemak,";
    options = "grp:win_space_toggle";
  };

  console.keyMap = ./colemak.map;

  # Fallback to initrd console configuration
  systemd.services = {
    systemd-vconsole-setup.enable = false;
    reload-systemd-vconsole-setup.enable = false;
  };
}

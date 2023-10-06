{ ... }:

{
  hardware.bluetooth.enable = true;

  # ConfigurationDirectory 'bluetooth' already exists but the mode is different. (File system: 755 ConfigurationDirectoryMode: 555)
  systemd.services.bluetooth.serviceConfig.ConfigurationDirectoryMode = "0755";
}

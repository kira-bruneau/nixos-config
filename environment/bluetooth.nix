{ config, ... }:

{
  hardware.bluetooth = {
    enable = true;

    # Disable plugins that require experimental features
    disabledPlugins = [ "bap" "mcp" "vcp" ];
  };

  # ConfigurationDirectory 'bluetooth' already exists but the mode is different. (File system: 755 ConfigurationDirectoryMode: 555)
  systemd.services.bluetooth.serviceConfig.ConfigurationDirectoryMode = "0755";

  # Disable auto switching to headset profile
  environment.etc."wireplumber/policy.lua.d/11-bluetooth-policy.lua" = {
    enable = config.services.pipewire.wireplumber.enable;
    text = ''
      bluetooth_policy.policy["media-role.use-headset-profile"] = false
    '';
  };
}

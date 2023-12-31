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

  # WirePlumber device configuration
  environment.etc."wireplumber/bluetooth.lua.d/51-bluez-config.lua" = {
    enable = config.services.pipewire.wireplumber.enable;
    text = ''
      table.insert(bluez_monitor.rules, {
        matches = {
          {
            { "device.name", "equals", "bluez_card.C8_7B_23_4B_27_6E" },
          },
        },
        apply_properties = {
          ["device.description"] = "Headphones",
          ["device.nick"] = "Headphones",
        },
      })

      table.insert(bluez_monitor.rules, {
        matches = {
          {
            { "node.name", "equals", "bluez_output.C8_7B_23_4B_27_6E.1" },
          },
        },
        apply_properties = {
          ["node.description"] = "Headphones",
          ["node.nick"] = "Headphones",
          ["bluez5.auto-connect"] = "[ a2dp_sink ]",
        },
      })
    '';
  };
}

{ inputs, pkgsKiraNur, ... }:

{
  imports = [
    inputs.kira-nur.nixosModules.bluetooth-autoconnect
  ];

  hardware.bluetooth = {
    enable = true;

    # Disable plugins that require experimental features
    disabledPlugins = [
      "asha"
      "bap"
      "bass"
      "ccp"
      "csip"
      "mcp"
      "micp"
      "vcp"
    ];
  };

  # ConfigurationDirectory 'bluetooth' already exists but the mode is different. (File system: 755 ConfigurationDirectoryMode: 555)
  systemd.services.bluetooth.serviceConfig.ConfigurationDirectoryMode = "0755";

  services.pipewire.wireplumber.extraConfig = {
    bluetooth = {
      "wireplumber.settings" = {
        "bluetooth.autoswitch-to-headset-profile" = false;
      };

      "monitor.bluez.rules" = [
        {
          matches = [ { "device.name" = "bluez_card.C8_7B_23_4B_27_6E"; } ];
          actions = {
            update-props = {
              "device.nick" = "Headphones";
              "device.description" = "Headphones";
            };
          };
        }
        {
          matches = [ { "node.name" = "bluez_output.C8_7B_23_4B_27_6E.1"; } ];
          actions = {
            update-props = {
              "node.nick" = "Headphones";
              "node.description" = "Headphones";
              "bluez5.auto-connect" = [ "a2dp_sink" ];
            };
          };
        }
      ];
    };
  };

  services.bluetooth-autoconnect = {
    enable = true;
    package = pkgsKiraNur.bluetooth-autoconnect;
  };
}

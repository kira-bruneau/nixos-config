{ inputs, config, ... }:

{
  imports = [
    inputs.home-manager.nixosModules.default
    ../group/audio.nix
  ];

  users.users.kira = {
    isNormalUser = true;
    description = "Kira Bruneau";
    extraGroups = [ "wheel" "adbusers" "audio" "video" ];
    initialPassword = "kira";
  };

  home-manager = {
    useUserPackages = true;
    users.kira = inputs.home-config-kira.nixosModules.${config.networking.hostName};
  };
}

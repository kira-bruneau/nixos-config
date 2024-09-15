{
  config,
  inputs,
  lib,
  pkgsKiraNur,
  ...
}:

{
  imports = [
    ../environments/autologin.nix
    ../environments/gaming.nix
    ../environments/gui/gnome.nix
    ../users/jakira.nix
    inputs.jovian.nixosModules.default
  ];

  system.stateVersion = "24.05";

  users.defaultUser = "jakira";

  jovian = {
    steam = {
      enable = true;
      autoStart = true;
      user = "jakira";
      desktopSession = "gnome";
    };

    decky-loader.enable = true;
  };

  systemd.services.decky-loader = {
    preStart = ''
      ln -snf ${pkgsKiraNur.powertools} ${config.jovian.decky-loader.stateDir}/plugins/PowerTools
    '';
  };

  services.xserver.xkb = {
    layout = lib.mkForce "us,us";
    variant = lib.mkForce ",colemak";
  };
}

{ config, lib, ... }:

{
  imports = [ ./autologin.nix ];

  users.users.${config.users.defaultUser} = {
    password = config.users.defaultUser;
    hashedPasswordFile = lib.mkForce null;
  };
}

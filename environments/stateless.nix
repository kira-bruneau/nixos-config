{ lib, ... }:

{
  imports = [
    ./autologin.nix
  ];

  users.users.kira = {
    password = "kira";
    hashedPasswordFile = lib.mkForce null;
  };
}

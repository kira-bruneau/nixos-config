{ lib, ... }:

{
  options = {
    users.defaultUser = lib.mkOption {
      type = lib.types.str;
      description = "Default account for autologin.";
    };
  };
}

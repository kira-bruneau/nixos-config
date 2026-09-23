{ config, ... }:

{
  system.stateVersion = config.system.nixos.release;

  users.defaultUser = "root";
}

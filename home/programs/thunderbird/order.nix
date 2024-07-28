# https://github.com/nix-community/home-manager/pull/4870

{ lib, ... }:

with lib;

{
  options = {
    accounts.email.order = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Order of email accounts from [](#opt-accounts.email.accounts).";
    };
  };
}

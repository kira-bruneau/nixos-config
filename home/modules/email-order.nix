# https://github.com/nix-community/home-manager/pull/4870

{ lib, ... }:

let
  inherit (lib) types;
in
{
  options = {
    accounts.email.order = lib.mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Order of email accounts from [](#opt-accounts.email.accounts).";
    };
  };
}

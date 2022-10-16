{ ... }:

{
  imports = [
    ./wireless.nix
  ];

  location.provider = "geoclue2";

  services.localtimed.enable = true;
}

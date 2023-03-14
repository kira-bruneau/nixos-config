{ ... }:

{
  imports = [
    ./wifi.nix
    ./bluetooth.nix
  ];

  location.provider = "geoclue2";

  services.localtimed.enable = true;
}

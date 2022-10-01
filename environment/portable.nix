{ ... }:

{
  imports = [
    ./wireless.nix
  ];

  services.localtimed.enable = true;
}

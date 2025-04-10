{ config, lib, ... }:

{
  nix.settings.trusted-public-keys = [ "amethyst:wWzYeKRMtWixW1rMNwf4jG+wWPUwRMEHCEB5WKixoes=" ];

  networking.hosts."100.64.0.6" = lib.mkIf config.services.tailscale.enable [ "amethyst" ];

  programs.ssh.knownHosts.amethyst.publicKey =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEt3gfHRXW9WP0/wff2WEVGqlr/6b6jtr6fpz9uRkotS";

  services.syncthing.settings = {
    devices.amethyst.id = "2PIQVSQ-2N77DGJ-XNTHNQF-PREKTRC-SCP6LFV-DRG3WK7-WFPT56T-NYWIAQG";

    folders = {
      "Auth".devices = [ "amethyst" ];
      "Dev".devices = [ "amethyst" ];
      "Documents".devices = [ "amethyst" ];
      "Pictures".devices = [ "amethyst" ];
      "RSS".devices = [ "amethyst" ];
      "Videos".devices = [ "amethyst" ];
    };
  };
}

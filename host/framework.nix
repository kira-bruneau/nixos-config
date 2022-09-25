{ lib, pkgs, ... }:

{
  imports = [
    ../environment/cli.nix
    ../environment/gui.nix
    ../package/arctype
  ];

  home.packages = with pkgs; [
    vscodium
  ];

  services.syncthing.enable = true;

  programs.mpv.config = {
    # Hardware acceleration
    hwdec = "vaapi";

    # Fix stuttering playing 4k video
    hdr-compute-peak = "no";
  };

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "arctype"
    "unrar"
  ];

  home.stateVersion = "21.11";
}

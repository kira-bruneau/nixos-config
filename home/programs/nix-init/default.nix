{ pkgs, ... }:

let
  settingsFormat = pkgs.formats.toml { };
in
{
  home.packages = with pkgs; [
    nix-init
  ];

  xdg.configFile."nix-init/config.toml".source = settingsFormat.generate "config.toml" {
    maintainers = [ "kira-bruneau" ];
    commit = true;
  };
}

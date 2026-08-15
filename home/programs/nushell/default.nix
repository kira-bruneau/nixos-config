{ pkgs, ... }:

{
  imports = [ ../carapace ];

  programs.nushell = {
    enable = true;
    plugins = with pkgs.nushellPlugins; [
      desktop_notifications
      skim
    ];
  };
}

{ pkgsUnstable, pkgsKiraNur, ... }:

{
  imports = [ ../carapace ];

  programs.nushell = {
    enable = true;
    package = pkgsKiraNur.nushell-colemak;
    plugins = with pkgsUnstable.nushellPlugins; [
      desktop_notifications
      skim
    ];
  };
}

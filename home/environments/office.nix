{ pkgs, ... }:

{
  imports = [ ../programs/anytype ];

  home.packages = with pkgs; [
    gnucash
    libreoffice
    xournalpp
  ];
}

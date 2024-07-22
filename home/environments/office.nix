{ pkgs, ... }:

{
  imports = [
    ../programs/anytype
    ../programs/protonmail-bridge
  ];

  home.packages = with pkgs; [
    gnucash
    libreoffice
    xournalpp
  ];
}

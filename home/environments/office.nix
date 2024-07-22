{ pkgs, ... }:

{
  imports = [
    ../programs/anytype
    ../programs/protonmail-bridge
    ../programs/thunderbird
  ];

  home.packages = with pkgs; [
    gnucash
    libreoffice
    xournalpp
  ];
}

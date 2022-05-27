{ lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    arctype
  ];

  xdg.mimeApps.defaultApplications = {
    "x-scheme-handler/arctype" = "arctype.desktop";
  };
}

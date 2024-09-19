{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [ rmg ];
  };
}

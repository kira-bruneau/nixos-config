{
  writeShellScript,
  findutils,
  coreutils,
}:

writeShellScript "random-wallpaper" ''
  ${findutils}/bin/find -L ~/Pictures/Wallpapers -type f | ${coreutils}/bin/shuf -n 1
''

{ lib }:

{
  allowUnfreePredicate =
    pkg:
    builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
      "2ship2harkinian"
      "anytype"
      "anytype-heart"
      "aspell-dict-en-science"
      "broadcom-sta"
      "clonehero"
      "clonehero-unwrapped"
      "data.zip" # vvvvvv
      "discord"
      "minecraft-server"
      "nvidia-settings"
      "nvidia-x11"
      "sm64ex"
      "steam"
      "steam-jupiter-original"
      "steam-jupiter-unwrapped"
      "steam-original"
      "steam-unwrapped"
      "steamdeck-hw-theme"
      "unrar"
      "vvvvvv"
      "mongodb-ce"
    ];

  permittedInsecurePackages = [
    # sonarr: https://github.com/NixOS/nixpkgs/issues/360592
    "aspnetcore-runtime-6.0.36"
    "aspnetcore-runtime-wrapped-6.0.36"
    "dotnet-sdk-6.0.428"
    "dotnet-sdk-wrapped-6.0.428"

    # anytype
    "electron-29.4.6"
  ];
}

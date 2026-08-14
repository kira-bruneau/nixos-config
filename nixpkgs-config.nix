{ lib }:

{
  allowUnfreePredicate =
    pkg:
    builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
      "2ship2harkinian"
      "anytype"
      "anytype-heart"
      "aspell-dict-en-science"
      "clonehero"
      "data.zip" # vvvvvv
      "discord"
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
    # mautrix-discord, mautrix-whatsapp
    "olm-3.2.16"
  ];
}

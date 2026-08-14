{ lib }:

{
  allowUnfreePredicate =
    pkg:
    builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
      "2ship2harkinian"
      "anytype" # anytype license (non-commercial, source-available)
      "anytype-heart" # anytype license (non-commercial, source-available)
      "aspell-dict-en-science" # no explicit license
      "clonehero"
      "data.zip" # vvvvvv
      "discord"
      "mongodb-ce" # sspl license (AGPL + source of all software used in hosting must be made available)
      "sm64ex"
      "steam"
      "steam-jupiter-original"
      "steam-jupiter-unwrapped"
      "steam-original"
      "steam-unwrapped"
      "steamdeck-hw-theme"
      "unrar" # unrar license (source may not be used to develop a compatible archiver)
      "vvvvvv"
    ];

  permittedInsecurePackages = [
    # mautrix-discord, mautrix-whatsapp
    "olm-3.2.16"
  ];
}

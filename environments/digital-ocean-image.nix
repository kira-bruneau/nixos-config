{
  modulesPath,
  ...
}:

{
  imports = [
    "${toString modulesPath}/virtualisation/digital-ocean-image.nix"
    ./stateless.nix
  ];

  virtualisation.digitalOceanImage.compressionMethod = "bzip2";
}

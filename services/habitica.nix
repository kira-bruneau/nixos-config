{
  inputs,
  pkgs,
  pkgsKiraNur,
  ...
}:

{
  imports = [
    inputs.kira-nur.nixosModules.habitica
  ];

  services = {
    habitica = {
      enable = true;
      package = pkgsKiraNur.jakirica;
      hostName = "habitica.jakira.space";
    };

    mongodb.package = pkgs.mongodb-ce;
  };
}

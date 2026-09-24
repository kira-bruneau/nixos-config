{
  users = {
    users.builder = {
      isSystemUser = true;
      useDefaultShell = true;
      group = "builder";
    };

    groups.builder = { };
  };

  nix.settings.keep-outputs = true;
}

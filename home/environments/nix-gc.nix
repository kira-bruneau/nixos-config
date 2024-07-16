{
  nix.gc = {
    automatic = true;
    frequency = "weekly";
    options = "--delete-older-than 30d";
  };

  # TODO: Use module option once 24.11 is released
  systemd.user.timers.nix-gc.Timer.Persistent = true;
}

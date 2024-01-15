{ config, ... }:

{
  services.kubo = {
    enable = true;
    startWhenNeeded = true;
  };
}

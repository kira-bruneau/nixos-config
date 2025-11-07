{ lib, ... }:

{
  imports = [ ./kira.nix ];

  home-manager.users.kira = {
    programs.git.settings = {
      user.email = lib.mkForce "kira@evokehealth.ca";
      gitlab.user = lib.mkForce "kira-evokehealth";
      github.user = lib.mkForce "kira-evokehealth";
    };
  };
}

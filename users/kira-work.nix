{ lib, ... }:

{
  imports = [ ./kira.nix ];

  home-manager.users.kira = {
    programs.git = {
      userEmail = lib.mkForce "kira@evokehealth.ca";
      extraConfig = {
        gitlab.user = lib.mkForce "kira-evokehealth";
        github.user = lib.mkForce "kira-evokehealth";
      };
    };
  };
}

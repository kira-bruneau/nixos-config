{
  lib,
  config,
  pkgsUnstable,
  ...
}:

{
  programs.thunderbird = {
    enable = true;
    package = pkgsUnstable.thunderbird-128;
    profiles = {
      thunderbird = {
        isDefault = true;
        settings = {
          # General
          "mail.rights.version" = 1;
          "mail.shell.checkDefaultClient" = false;
          "mailnews.start_page.enabled" = false;

          # Phishing detection
          "mail.phishing.detection.enabled" = false;

          # Telemetry
          "datareporting.healthreport.uploadEnabled" = false;
          "datareporting.policy.dataSubmissionEnabled" = false;
        };
      };
    };
  };

  wayland.windowManager.sway.config = {
    startup = [ { command = lib.getExe config.programs.thunderbird.package; } ];
    assigns."9" = [ { app_id = "^thunderbird$"; } ];
  };

  xdg.mimeApps.defaultApplications = {
    "message/rfc822" = "thunderbird.desktop";
    "x-scheme-handler/mailto" = "thunderbird.desktop";
    "x-scheme-handler/mid" = "thunderbird.desktop";
  };
}

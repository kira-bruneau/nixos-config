{
  config,
  lib,
  pkgs,
  ...
}:

{
  # Quiet boot
  # FIXME: This still shows fsck messages & NixOS messages from stage-1-init.sh & stage-2-init.sh scripts
  boot = {
    initrd.verbose = false;
    consoleLogLevel = 3;
    kernelParams = [
      "quiet"
      "rd.udev.log_level=3"
    ];
  };

  environment = {
    # Force all apps to use the same version of mesa as in hardware.graphics.package,
    # regardless of the version it was compiled with
    extraInit = lib.mkIf config.hardware.graphics.enable ''
      export LD_LIBRARY_PATH=$LD_LIBRARY_PATH''${LD_LIBRARY_PATH:+:}${
        lib.makeLibraryPath [
          config.hardware.graphics.package.out
          config.hardware.graphics.package32.out
        ]
      }
    '';

    sessionVariables = {
      NAUTILUS_4_EXTENSION_DIR = "${pkgs.nautilus-python}/lib/nautilus/extensions-4";
    };

    pathsToLink = [
      "/share/nautilus-python/extensions"
    ];
  };

  # Let the desktop environment handle the power key
  services.logind.extraConfig = "HandlePowerKey=ignore";

  # Enable GUI for managing bluetooth
  services.blueman.enable = config.hardware.bluetooth.enable;

  # Enable automatic device mounting
  services.devmon.enable = true;

  # Disable bitmap fonts
  fonts.fontconfig.allowBitmaps = false;

  # Reduce priority of nix daemon on desktop systems so the system is
  # still usable while a nix build is running
  nix = {
    daemonCPUSchedPolicy = "idle";
    daemonIOSchedClass = "idle";
  };

  # Generate documentation for all imported modules
  documentation.nixos.includeAllModules = true;
}

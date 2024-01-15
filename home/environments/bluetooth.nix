{
  services.blueman-applet.enable = true;

  systemd.user.services.blueman-applet.Service.Environment = [
    # Workaround bluman-applet not getting input
    # https://bugreports.qt.io/browse/QTBUG-49952
    "QT_XCB_NO_XI2_MOUSE=1"
  ];

  # Forward bluetooth media controls to MPRIS
  services.mpris-proxy.enable = true;
}

{ pkgs, ... }:

{
  programs.mpv = {
    enable = true;
    config = {
      save-position-on-quit = true;
    };

    scripts = with pkgs.mpvScripts; [ mpris ];
  };

  home.packages = with pkgs; [ celluloid ];

  xdg.mimeApps.defaultApplications = {
    "application/mxf" = "celluloid.desktop";
    "application/ogg" = "celluloid.desktop";
    "application/sdp" = "celluloid.desktop";
    "application/smil" = "celluloid.desktop";
    "application/streamingmedia" = "celluloid.desktop";
    "application/vnd.apple.mpegurl" = "celluloid.desktop";
    "application/vnd.ms-asf" = "celluloid.desktop";
    "application/vnd.rn-realmedia" = "celluloid.desktop";
    "application/vnd.rn-realmedia-vbr" = "celluloid.desktop";
    "application/x-cue" = "celluloid.desktop";
    "application/x-extension-m4a" = "celluloid.desktop";
    "application/x-extension-mp4" = "celluloid.desktop";
    "application/x-matroska" = "celluloid.desktop";
    "application/x-mpegurl" = "celluloid.desktop";
    "application/x-ogg" = "celluloid.desktop";
    "application/x-ogm" = "celluloid.desktop";
    "application/x-ogm-audio" = "celluloid.desktop";
    "application/x-ogm-video" = "celluloid.desktop";
    "application/x-shorten" = "celluloid.desktop";
    "application/x-smil" = "celluloid.desktop";
    "application/x-streamingmedia" = "celluloid.desktop";
    "audio/3gpp" = "celluloid.desktop";
    "audio/3gpp2" = "celluloid.desktop";
    "audio/aac" = "celluloid.desktop";
    "audio/ac3" = "celluloid.desktop";
    "audio/aiff" = "celluloid.desktop";
    "audio/AMR" = "celluloid.desktop";
    "audio/amr-wb" = "celluloid.desktop";
    "audio/dv" = "celluloid.desktop";
    "audio/eac3" = "celluloid.desktop";
    "audio/flac" = "celluloid.desktop";
    "audio/m3u" = "celluloid.desktop";
    "audio/m4a" = "celluloid.desktop";
    "audio/mp1" = "celluloid.desktop";
    "audio/mp2" = "celluloid.desktop";
    "audio/mp3" = "celluloid.desktop";
    "audio/mp4" = "celluloid.desktop";
    "audio/mpeg" = "celluloid.desktop";
    "audio/mpeg2" = "celluloid.desktop";
    "audio/mpeg3" = "celluloid.desktop";
    "audio/mpegurl" = "celluloid.desktop";
    "audio/mpg" = "celluloid.desktop";
    "audio/musepack" = "celluloid.desktop";
    "audio/ogg" = "celluloid.desktop";
    "audio/opus" = "celluloid.desktop";
    "audio/rn-mpeg" = "celluloid.desktop";
    "audio/scpls" = "celluloid.desktop";
    "audio/vnd.dolby.heaac.1" = "celluloid.desktop";
    "audio/vnd.dolby.heaac.2" = "celluloid.desktop";
    "audio/vnd.dts" = "celluloid.desktop";
    "audio/vnd.dts.hd" = "celluloid.desktop";
    "audio/vnd.rn-realaudio" = "celluloid.desktop";
    "audio/vorbis" = "celluloid.desktop";
    "audio/wav" = "celluloid.desktop";
    "audio/webm" = "celluloid.desktop";
    "audio/x-aac" = "celluloid.desktop";
    "audio/x-adpcm" = "celluloid.desktop";
    "audio/x-aiff" = "celluloid.desktop";
    "audio/x-ape" = "celluloid.desktop";
    "audio/x-m4a" = "celluloid.desktop";
    "audio/x-matroska" = "celluloid.desktop";
    "audio/x-mp1" = "celluloid.desktop";
    "audio/x-mp2" = "celluloid.desktop";
    "audio/x-mp3" = "celluloid.desktop";
    "audio/x-mpegurl" = "celluloid.desktop";
    "audio/x-mpg" = "celluloid.desktop";
    "audio/x-ms-asf" = "celluloid.desktop";
    "audio/x-ms-wma" = "celluloid.desktop";
    "audio/x-musepack" = "celluloid.desktop";
    "audio/x-pls" = "celluloid.desktop";
    "audio/x-pn-au" = "celluloid.desktop";
    "audio/x-pn-realaudio" = "celluloid.desktop";
    "audio/x-pn-wav" = "celluloid.desktop";
    "audio/x-pn-windows-pcm" = "celluloid.desktop";
    "audio/x-realaudio" = "celluloid.desktop";
    "audio/x-scpls" = "celluloid.desktop";
    "audio/x-shorten" = "celluloid.desktop";
    "audio/x-tta" = "celluloid.desktop";
    "audio/x-vorbis" = "celluloid.desktop";
    "audio/x-vorbis+ogg" = "celluloid.desktop";
    "audio/x-wav" = "celluloid.desktop";
    "audio/x-wavpack" = "celluloid.desktop";
    "video/3gp" = "celluloid.desktop";
    "video/3gpp" = "celluloid.desktop";
    "video/3gpp2" = "celluloid.desktop";
    "video/avi" = "celluloid.desktop";
    "video/divx" = "celluloid.desktop";
    "video/dv" = "celluloid.desktop";
    "video/fli" = "celluloid.desktop";
    "video/flv" = "celluloid.desktop";
    "video/mkv" = "celluloid.desktop";
    "video/mp2t" = "celluloid.desktop";
    "video/mp4" = "celluloid.desktop";
    "video/mp4v-es" = "celluloid.desktop";
    "video/mpeg" = "celluloid.desktop";
    "video/msvideo" = "celluloid.desktop";
    "video/ogg" = "celluloid.desktop";
    "video/quicktime" = "celluloid.desktop";
    "video/vnd.divx" = "celluloid.desktop";
    "video/vnd.mpegurl" = "celluloid.desktop";
    "video/vnd.rn-realvideo" = "celluloid.desktop";
    "video/webm" = "celluloid.desktop";
    "video/x-avi" = "celluloid.desktop";
    "video/x-flc" = "celluloid.desktop";
    "video/x-flic" = "celluloid.desktop";
    "video/x-flv" = "celluloid.desktop";
    "video/x-m4v" = "celluloid.desktop";
    "video/x-matroska" = "celluloid.desktop";
    "video/x-mpeg2" = "celluloid.desktop";
    "video/x-mpeg3" = "celluloid.desktop";
    "video/x-ms-afs" = "celluloid.desktop";
    "video/x-ms-asf" = "celluloid.desktop";
    "video/x-ms-wmv" = "celluloid.desktop";
    "video/x-ms-wmx" = "celluloid.desktop";
    "video/x-ms-wvxvideo" = "celluloid.desktop";
    "video/x-msvideo" = "celluloid.desktop";
    "video/x-ogm" = "celluloid.desktop";
    "video/x-ogm+ogg" = "celluloid.desktop";
    "video/x-theora" = "celluloid.desktop";
    "video/x-theora+ogg" = "celluloid.desktop";
  };

  programs.niri.settings = {
    window-rules = [
      {
        matches = [ { app-id = "^mpv$"; } ];
        open-on-workspace = "1-browsing";
        open-fullscreen = true;
      }
      {
        matches = [ { app-id = "^io.github.celluloid_player.Celluloid$"; } ];
        open-on-workspace = "1-browsing";
        open-fullscreen = true;
      }
    ];
  };
}

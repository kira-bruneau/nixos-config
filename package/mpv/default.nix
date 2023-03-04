{ pkgs, ... }:

{
  programs.mpv = {
    enable = true;
    config = {
      save-position-on-quit = true;
    };
  };

  xdg.mimeApps.defaultApplications = {
    "application/mxf" = "mpv.desktop";
    "application/ogg" = "mpv.desktop";
    "application/sdp" = "mpv.desktop";
    "application/smil" = "mpv.desktop";
    "application/streamingmedia" = "mpv.desktop";
    "application/vnd.apple.mpegurl" = "mpv.desktop";
    "application/vnd.ms-asf" = "mpv.desktop";
    "application/vnd.rn-realmedia" = "mpv.desktop";
    "application/vnd.rn-realmedia-vbr" = "mpv.desktop";
    "application/x-cue" = "mpv.desktop";
    "application/x-extension-m4a" = "mpv.desktop";
    "application/x-extension-mp4" = "mpv.desktop";
    "application/x-matroska" = "mpv.desktop";
    "application/x-mpegurl" = "mpv.desktop";
    "application/x-ogg" = "mpv.desktop";
    "application/x-ogm" = "mpv.desktop";
    "application/x-ogm-audio" = "mpv.desktop";
    "application/x-ogm-video" = "mpv.desktop";
    "application/x-shorten" = "mpv.desktop";
    "application/x-smil" = "mpv.desktop";
    "application/x-streamingmedia" = "mpv.desktop";
    "audio/3gpp" = "mpv.desktop";
    "audio/3gpp2" = "mpv.desktop";
    "audio/aac" = "mpv.desktop";
    "audio/ac3" = "mpv.desktop";
    "audio/aiff" = "mpv.desktop";
    "audio/AMR" = "mpv.desktop";
    "audio/amr-wb" = "mpv.desktop";
    "audio/dv" = "mpv.desktop";
    "audio/eac3" = "mpv.desktop";
    "audio/flac" = "mpv.desktop";
    "audio/m3u" = "mpv.desktop";
    "audio/m4a" = "mpv.desktop";
    "audio/mp1" = "mpv.desktop";
    "audio/mp2" = "mpv.desktop";
    "audio/mp3" = "mpv.desktop";
    "audio/mp4" = "mpv.desktop";
    "audio/mpeg" = "mpv.desktop";
    "audio/mpeg2" = "mpv.desktop";
    "audio/mpeg3" = "mpv.desktop";
    "audio/mpegurl" = "mpv.desktop";
    "audio/mpg" = "mpv.desktop";
    "audio/musepack" = "mpv.desktop";
    "audio/ogg" = "mpv.desktop";
    "audio/opus" = "mpv.desktop";
    "audio/rn-mpeg" = "mpv.desktop";
    "audio/scpls" = "mpv.desktop";
    "audio/vnd.dolby.heaac.1" = "mpv.desktop";
    "audio/vnd.dolby.heaac.2" = "mpv.desktop";
    "audio/vnd.dts" = "mpv.desktop";
    "audio/vnd.dts.hd" = "mpv.desktop";
    "audio/vnd.rn-realaudio" = "mpv.desktop";
    "audio/vorbis" = "mpv.desktop";
    "audio/wav" = "mpv.desktop";
    "audio/webm" = "mpv.desktop";
    "audio/x-aac" = "mpv.desktop";
    "audio/x-adpcm" = "mpv.desktop";
    "audio/x-aiff" = "mpv.desktop";
    "audio/x-ape" = "mpv.desktop";
    "audio/x-m4a" = "mpv.desktop";
    "audio/x-matroska" = "mpv.desktop";
    "audio/x-mp1" = "mpv.desktop";
    "audio/x-mp2" = "mpv.desktop";
    "audio/x-mp3" = "mpv.desktop";
    "audio/x-mpegurl" = "mpv.desktop";
    "audio/x-mpg" = "mpv.desktop";
    "audio/x-ms-asf" = "mpv.desktop";
    "audio/x-ms-wma" = "mpv.desktop";
    "audio/x-musepack" = "mpv.desktop";
    "audio/x-pls" = "mpv.desktop";
    "audio/x-pn-au" = "mpv.desktop";
    "audio/x-pn-realaudio" = "mpv.desktop";
    "audio/x-pn-wav" = "mpv.desktop";
    "audio/x-pn-windows-pcm" = "mpv.desktop";
    "audio/x-realaudio" = "mpv.desktop";
    "audio/x-scpls" = "mpv.desktop";
    "audio/x-shorten" = "mpv.desktop";
    "audio/x-tta" = "mpv.desktop";
    "audio/x-vorbis" = "mpv.desktop";
    "audio/x-vorbis+ogg" = "mpv.desktop";
    "audio/x-wav" = "mpv.desktop";
    "audio/x-wavpack" = "mpv.desktop";
    "video/3gp" = "mpv.desktop";
    "video/3gpp" = "mpv.desktop";
    "video/3gpp2" = "mpv.desktop";
    "video/avi" = "mpv.desktop";
    "video/divx" = "mpv.desktop";
    "video/dv" = "mpv.desktop";
    "video/fli" = "mpv.desktop";
    "video/flv" = "mpv.desktop";
    "video/mkv" = "mpv.desktop";
    "video/mp2t" = "mpv.desktop";
    "video/mp4" = "mpv.desktop";
    "video/mp4v-es" = "mpv.desktop";
    "video/mpeg" = "mpv.desktop";
    "video/msvideo" = "mpv.desktop";
    "video/ogg" = "mpv.desktop";
    "video/quicktime" = "mpv.desktop";
    "video/vnd.divx" = "mpv.desktop";
    "video/vnd.mpegurl" = "mpv.desktop";
    "video/vnd.rn-realvideo" = "mpv.desktop";
    "video/webm" = "mpv.desktop";
    "video/x-avi" = "mpv.desktop";
    "video/x-flc" = "mpv.desktop";
    "video/x-flic" = "mpv.desktop";
    "video/x-flv" = "mpv.desktop";
    "video/x-m4v" = "mpv.desktop";
    "video/x-matroska" = "mpv.desktop";
    "video/x-mpeg2" = "mpv.desktop";
    "video/x-mpeg3" = "mpv.desktop";
    "video/x-ms-afs" = "mpv.desktop";
    "video/x-ms-asf" = "mpv.desktop";
    "video/x-ms-wmv" = "mpv.desktop";
    "video/x-ms-wmx" = "mpv.desktop";
    "video/x-ms-wvxvideo" = "mpv.desktop";
    "video/x-msvideo" = "mpv.desktop";
    "video/x-ogm" = "mpv.desktop";
    "video/x-ogm+ogg" = "mpv.desktop";
    "video/x-theora" = "mpv.desktop";
    "video/x-theora+ogg" = "mpv.desktop";
  };
}

{
  lib,
  writers,
  mozlz4a,
}:

writers.writeNuBin "ff-search"
  {
    makeWrapperArgs = [
      "--prefix"
      "PATH"
      ":"
      "${lib.makeBinPath [ mozlz4a ]}"
    ];
  }
  ''
    export def main [
      file?: string
      --json (-j)
    ] {
      if $file != null { open --raw $file } else { $in } |
      ^mozlz4a -d - |
      from json |
      if $json { to json } else { $in }
    }
  ''

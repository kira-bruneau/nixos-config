{
  programs.git = {
    enable = true;
    extraConfig = {
      init.defaultBranch = "main";

      core = {
        fsmonitor = true;
        whitespace = "cr-at-eol";
      };

      pull.rebase = "merges";
      rebase.autostash = true;
      diff.algorithm = "histogram";

      "url \"git@gitlab.com:\"" = {
        pushInsteadOf = [
          "git://gitlab.com/"
          "https://gitlab.com/"
        ];
      };

      "url \"git@github.com:\"" = {
        pushInsteadOf = [
          "git://github.com/"
          "https://github.com/"
        ];
      };
    };

    ignores = [
      # Emacs lock files
      ".[#]*"

      # Emacs autosave files
      "[#]*[#]"

      # Emacs backup files
      "*~"

      # Project Management
      ".direnv"
      ".idea"
      ".mypy_cache"
      "compile_commands.json"

      ## Eclipse JDT Language Server
      ".classpath"
      ".project"
      ".settings"
      ".factorypath"
    ];
  };
}

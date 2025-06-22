{
  programs.git = {
    enable = true;
    extraConfig = {
      init.defaultBranch = "main";

      core = {
        fsmonitor = true;
        whitespace = "cr-at-eol";
      };

      diff.algorithm = "histogram";
      pull.rebase = "merges";
      rebase.autostash = true;

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

      # Nix result files
      "result"
      "result-*"

      # Project Management
      ".cache/clangd"
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

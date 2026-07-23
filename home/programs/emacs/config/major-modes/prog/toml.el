;; toml-ts-mode hangs on large files, so use conf-toml-mode instead
(use-package conf-toml-mode
  :mode "\\(\\.toml\\|/\\(Cargo.lock\\|poetry.lock\\)\\)\\'")

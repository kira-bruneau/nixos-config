((magit-diff:magit-status-mode "--no-ext-diff")
 (magit-log:magit-log-mode "-n256" "--graph" "--color" "--decorate" "--follow")
 (rg-menu "--hidden" "--follow" "--glob=!.git"))

(straight-use-package 'sql-indent)

(eval-after-load "sql"
  '(load-library "sql-indent"))

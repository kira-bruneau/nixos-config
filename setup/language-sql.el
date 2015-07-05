(require-package
 '(sql-indent))

(eval-after-load "sql"
  '(load-library "sql-indent"))

(provide 'language-sql)

(require-package
 '(company-c-headers))

(setq gdb-many-windows t)

(setq flycheck-clang-include-path (list "/usr/include"))

(defun pkg-config-includes (extern-libs)
  (mapcar
   (lambda (include)
     (replace-regexp-in-string "^-I" "" include))
   (split-string
    (shell-command-to-string
     (concat
      "pkg-config --cflags-only-I "
      (mapconcat 'symbol-name extern-libs " "))))))

(provide 'language-c)

(require-package
 '(company-c-headers))

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-c-headers))

(setq gdb-many-windows t)

;; TODO: Handle errors (pkg-config not installed, invalid library, etc...)
(defun pkg-config-includes (extern-libs)
  "Obtain a list of include directories from pkg-config"
  (mapcar
   (lambda (include)
     (replace-regexp-in-string "^-I" "" include))
   (split-string
    (shell-command-to-string
     (concat
      "pkg-config --cflags-only-I "
      (mapconcat 'identity extern-libs " "))))))

(defcustom cproject-pkg-config nil
  "A list of pkg-config libraries to include in your C project"
  :type '(repeat string))

(defcustom cproject-include-path nil
  "A list of include directories to use in your C project"
  :type '(repeat string))

;; Read project configuration from .dir-locals.el
(add-hook 'c-mode-hook 'cproject-configure-locals)
(defun cproject-configure-locals ()
  (flycheck-mode t)
  (add-hook 'hack-local-variables-hook
            (lambda ()
              ;; Add include paths to flycheck
              (setq-local flycheck-clang-include-path
                          (append
                           (when (boundp 'flycheck-clang-include-path)
                             flycheck-clang-include-path)
                           (when (boundp 'cproject-include-path)
                             (mapcar
                              (lambda (include)
                                (expand-file-name include (locate-dominating-file default-directory ".dir-locals.el")))
                              cproject-include-path))
                           (when (boundp 'cproject-pkg-config)
                             (pkg-config-includes cproject-pkg-config))))

              ;; Add include paths to company
              (setq-local company-clang-arguments
                          (append
                           (when (boundp 'company-clang-arguments)
                             company-clang-arguments)
                           (when (boundp 'cproject-pkg-config)
                             (mapcar
                              (lambda (include)
                                (concat "-I" include))
                              (pkg-config-includes cproject-pkg-config))))))))

(provide 'language-c)

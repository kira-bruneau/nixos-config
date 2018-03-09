(use-package cc-mode
  :demand t
  ;; :ensure-system-package pkg-config
  :hook ((c-mode . cproject-configure-locals)
         (c-mode . flycheck-mode))
  :config
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
        (mapconcat 'shell-quote-argument extern-libs " "))))))

  (defun cproject-string-list-p (obj)
    "Determine if OBJ is a list of strings."
    (and (listp obj) (-all? 'stringp obj)))

  (defcustom cproject-pkg-config nil
    "A list of pkg-config libraries to include in your C project"
    :type '(repeat string)
    :safe 'cproject-string-list-p)

  (defcustom cproject-include-path nil
    "A list of include directories to use in your C project"
    :type '(repeat string)
    :safe 'cproject-string-list-p)

  ;; Read project configuration from .dir-locals.el
  (defun cproject-configure-locals ()
    (add-hook 'hack-local-variables-hook
              (lambda ()
                (cproject-configure-includes
                 (append

                  ;; Convert all `cproject-include-paths' to absolute paths
                  (when (boundp 'cproject-include-path)
                    (mapcar
                     (lambda (include)
                       (expand-file-name
                        include
                        (locate-dominating-file
                         default-directory ".dir-locals.el")))
                     cproject-include-path))

                  ;; Obtain all `cproject-pkg-config' includes with pkg-config
                  (when (boundp 'cproject-pkg-config)
                    (pkg-config-includes cproject-pkg-config)))))))

  (defun cproject-configure-includes (includes)
    (setq-local flycheck-clang-include-path includes)
    (setq-local company-clang-arguments
                (mapcar
                 (lambda (include)
                   (concat "-I" include))
                 includes))))

(use-package company-c-headers
  :straight t
  :commands (company-c-headers)
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-c-headers)))

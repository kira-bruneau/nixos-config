(defun my-editorconfig-hack-properties (props)
  ;; Ignore indent_size/tab_width when indent_style is tab
  (when (string-equal (gethash 'indent_style props) "tab")
    (remhash 'indent_size props)
    (remhash 'tab_width props)))

(use-package editorconfig
  :demand
  :config
  (add-hook 'editorconfig-hack-properties-functions
            #'my-editorconfig-hack-properties)

  (editorconfig-mode 1))

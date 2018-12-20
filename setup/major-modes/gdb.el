;; Source https://stackoverflow.com/questions/3860028/customizing-emacs-gdb#answer-41326527
(use-package gdb-mi
  :config
  (setq gdb-many-windows nil)

  (defun set-gdb-layout(&optional c-buffer)
    (if (not c-buffer)
        (setq c-buffer (window-buffer (selected-window)))) ;; save current buffer

    ;; from http://stackoverflow.com/q/39762833/846686
    (set-window-dedicated-p (selected-window) nil) ;; unset dedicate state if needed
    (switch-to-buffer gud-comint-buffer)
    (delete-other-windows) ;; clean all

    (let* ((w-source (selected-window)) ;; left top
           (w-gdb (split-window w-source nil 'right)) ;; right bottom
           (w-locals (split-window w-gdb nil 'above)) ;; right middle bottom
           (w-stack (split-window w-locals nil 'above)) ;; right middle top
           (w-breakpoints (split-window w-stack nil 'above)) ;; right top
           (w-io (split-window w-source (floor(* 0.9 (window-body-height)))
                               'below))) ;; left bottom
      (set-window-buffer w-io (gdb-get-buffer-create 'gdb-inferior-io))
      (set-window-dedicated-p w-io t)
      (set-window-buffer w-breakpoints (gdb-get-buffer-create 'gdb-breakpoints-buffer))
      (set-window-dedicated-p w-breakpoints t)
      (set-window-buffer w-locals (gdb-get-buffer-create 'gdb-locals-buffer))
      (set-window-dedicated-p w-locals t)
      (set-window-buffer w-stack (gdb-get-buffer-create 'gdb-stack-buffer))
      (set-window-dedicated-p w-stack t)

      (set-window-buffer w-gdb gud-comint-buffer)

      (select-window w-source)
      (set-window-buffer w-source c-buffer)))

  (defadvice gdb (around args activate)
    "Change the way to gdb works."
    (setq global-config-editing (current-window-configuration)) ;; to restore: (set-window-configuration c-editing)
    (let ((c-buffer (window-buffer (selected-window)))) ;; save current buffer
      ad-do-it
      (set-gdb-layout c-buffer)))

  (defadvice gdb-reset (around args activate)
    "Change the way to gdb exit."
    ad-do-it
    (set-window-configuration global-config-editing)))

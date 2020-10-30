(defun notify-async-shell-complete (process signal)
  (when (and
         (memq (process-status process) '(exit signal))
         (not (eq (process-buffer process) (window-buffer (selected-window)))))
    (make-process
     :name "notify-async-shell-complete"
     :command (list
               "notify-send"
               (format "Emacs %s" (buffer-name (process-buffer process)))
               (format "Exited with exit code %d" (process-exit-status process))))))

(with-eval-after-load 'simple
  (add-function :before (symbol-function 'shell-command-sentinel)
                #'notify-async-shell-complete))

(with-eval-after-load 'compile
  (add-function :before (symbol-function 'compilation-sentinel)
                #'notify-async-shell-complete))

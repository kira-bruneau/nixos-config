(require 'notifications)

(defun notify-async-shell-complete (process signal)
  (when (and
         (memq (process-status process) '(exit signal))
         ;; Don't notify if output buffer is visible
         (not
          (and
           (frame-focus-state)
           (get-buffer-window (process-buffer process) 'visible))))
    (notifications-notify
     :title (format "Emacs %s" (buffer-name (process-buffer process)))
     :body (format "Exited with exit code %d" (process-exit-status process))
     :urgency "low")))

(with-eval-after-load 'simple
  (add-function :before (symbol-function 'shell-command-sentinel)
                #'notify-async-shell-complete))

(with-eval-after-load 'compile
  (add-function :before (symbol-function 'compilation-sentinel)
                #'notify-async-shell-complete))

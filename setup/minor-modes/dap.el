(use-package dap-mode
  :straight t
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :init
  (require 'dap-gdb-lldb)
  (require 'dap-python)

  (setq treemacs-show-cursor t)

  (dap-mode t)
  (dap-tooltip-mode t)
  (dap-ui-mode t)

  (defhydra dap-hydra (:color pink :hint nil :foreign-keys run)
    "
^Stepping^          ^Switch^                 ^Breakpoints^           ^Eval
^^^^^^^^-----------------------------------------------------------------------------------------
_n_: Next           _ss_: Session            _bb_: Toggle            _ee_: Eval
_i_: Step in        _st_: Thread             _bd_: Delete            _er_: Eval region
_o_: Step out       _sf_: Stack frame        _ba_: Add               _es_: Eval thing at point
_c_: Continue       _sl_: List locals        _bc_: Set condition     _ea_: Add expression.
_r_: Restart        _sb_: List breakpoints   _bh_: Set hit count
_Q_: Disconnect     _sS_: List sessions      _bl_: Set log message
"
    ("n" dap-next)
    ("i" dap-step-in)
    ("o" dap-step-out)
    ("c" dap-continue)
    ("r" dap-debug-restart)
    ("ss" dap-switch-session)
    ("st" dap-switch-thread)
    ("sf" dap-switch-stack-frame)
    ("sl" dap-ui-locals)
    ("sb" dap-ui-breakpoints)
    ("sS" dap-ui-sessions)
    ("bb" dap-breakpoint-toggle)
    ("ba" dap-breakpoint-add)
    ("bd" dap-breakpoint-delete)
    ("bc" dap-breakpoint-condition)
    ("bh" dap-breakpoint-hit-condition)
    ("bl" dap-breakpoint-log-message)
    ("ee" dap-eval)
    ("ea" dap-ui-expressions-add)
    ("er" dap-eval-region)
    ("es" dap-eval-thing-at-point)
    ("q" nil "quit" :color blue)
    ("Q" dap-disconnect :color blue))

  (defun dap-hydra--self-insert ()
    (interactive)
    (message "self-insert disabled in dap-hydra"))

  (define-key dap-hydra/keymap [remap self-insert-command] #'dap-hydra--self-insert)
  (defun dap-hydra ()
    "Run `dap-hydra/body'."
    (interactive)
    (dap-hydra/body)))

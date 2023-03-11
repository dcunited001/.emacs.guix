(defun dc/current-terminal ()
  (let (current-terminal)
    (dolist (terminal (terminal-list))
      (when (and (eq t (terminal-live-p terminal))
                 (terminal-parameter terminal 'gpm-moue-active))
        (setq current-terminal terminal)))
    current-terminal))

;; (terminal-parameter (dc/current-terminal) 'gpm-mouse-active)

(provide 'dc-terminal)

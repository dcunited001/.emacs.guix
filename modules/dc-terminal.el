(require 't-mouse)

(defun dc/current-terminal (&optional term-list)
  (let ((term-list (or term-list (terminal-list)))
        (current-terminal))
    (dolist (term term-list)
      (when (eq t (terminal-live-p term))
        (setq-default current-terminal term)))
    current-terminal))

;; (terminal-parameter (dc/current-terminal) 'gpm-mouse-active)

(defun dc/other-terminals ()
  (seq-filter (lambda (term) (not (terminal-live-p term)))
              (terminal-list)))

;; returns a terminal where gpm is active or nil
(defun dc/gpm-active-on-terminals (&optional term-list)
  (let ((term-list (or term-list (terminal-list)))
        (active-terms))
    (dolist (term term-list)
      (when (terminal-parameter term 'gpm-mouse-active)
        (push term active-terms)))

    (when (> (length active-terms) 1)
      (warn "GPM reported active on multiple terminals")
      (warn "# active terms: %s" (pp (length active-terms)))
      (dolist (term active-terms)
        (warn "GPM active on %s" (pp-to-string term))))
    active-terms))

(defun dc/gpm-autostart ()
  (let* ((gpm-active-terms (dc/gpm-active-on-terminals)))
    (cond ((= 0 (length gpm-active-terms))
           (if gpm-mouse-mode
               (gpm-mouse-enable)
             (gpm-mouse-mode +1)))
          ((= 1 (length gpm-active-terms))
           (warn "GPM reportedly active on one terminal")))))

(add-hook 'emacs-startup-hook #'gpm-autostart)

(provide 'dc-terminal)

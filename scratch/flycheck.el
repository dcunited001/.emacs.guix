;; so basically, if there are no errors of a minimum-level,
;; flycheck-navigate-next-error will take you to the next error of any level.

;; #s(flycheck-error #<buffer flycheck.el<scratch>> emacs-lisp "/home/dc/.emacs.g/scratch/flycheck.el" 20 7 "assignment to free variable ‘dc/flycheck-error-temp’" warning nil nil nil nil)

;; level: warning
;; min-level: error
;; interesting? t
(defun dc/flycheck-error-level-interesting-at-pos-p (pos)
  "Check if error severity at POS passes `flycheck-error-level-interesting-p'."
  (interactive "d")
  (if-let ((err (get-char-property pos 'flycheck-error))
           (level (flycheck-error-level err))
           (interesting? (flycheck-error-level-interesting-p
                          (get-char-property pos 'flycheck-error))))
      (progn
        (pp err)
        (message "level: %s" level)
        (message "min-level: %s" flycheck-navigation-minimum-level)
        (message "interesting? %s" interesting?))))


;; (flycheck-has-errors-p flycheck-current-errors 'error) nil

;; an actual emacs-lisp error:
;; (defun dfsafml)

;; other misc code
(setq dc/flycheck-error-temp nil)
(defun dc/flycheck-get-error-at-pos (pos)
  "Return the flycheck error at `pos'."
  (interactive "d")
  (setq dc/flycheck-error-temp (get-char-property pos 'flycheck-error)))

(defun dc/flycheck-get-error-severity (&optional err)
  (interactive)
  (let ((err (or err dc/flycheck-error-temp)))
    (flycheck-error-level-severity (flycheck-error-level err))))


;; min-level: 100
;; (flycheck-error-level-severity (flycheck-error-level dc/flycheck-error-temp))
;; this-error: 10

(defun dc/flycheck-hide-emacs-lisp-highlights? (err &optional other-level)
  (let ((min-level flycheck-navigation-minimum-level)
        (this-level (flycheck-error-level err)))

    ;; min-level: error => nil
    (message "has current errors: %s"
             (flycheck-has-current-errors-p err))
    ;; min-level: error, other-level: nil => t
    ;; min-level: error, other-level: warning => t
    (message "has errors (with level): %s"
             (flycheck-has-errors-p flycheck-current-errors (or other-level min-level)))))

;; flycheck-error-level-interesting-p:
;; (when (flycheck-error-p err)
;;   (-if-let (min-level flycheck-navigation-minimum-level)
;;       (or (<= (flycheck-error-level-severity min-level)                     ;; 10
;;               (flycheck-error-level-severity (flycheck-error-level err)))   ;; 100
;;           (not (flycheck-has-current-errors-p min-level)))
;;     t))

;; flycheck-has-current-errors-p: if nil, error still interesting
;; (if level
;;     (flycheck-has-errors-p flycheck-current-errors level)
;;   (and flycheck-current-errors t))

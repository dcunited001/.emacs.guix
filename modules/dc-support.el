;; -*- lexical-binding: t; -*-

;;* Support

;;** Macros

;; NOTE: if parsing the body to extract bindings is necessary,
;; use macroexp-parse-body
(defmacro dc/toggleable-boolean (name &optional keybind)
  "Define an interactive defun to toggle the variable NAME
along with KEYBIND, if present"
  (declare (indent defun))
  (let* ((symname (symbol-name (or (intern-soft name) (defvar name))))
         (toggle-name (format "dc/toggle-%s" symname))
         (toggle-docstring (format "Toggle variable: %s" symname))
         (toggle-sym (or (intern-soft toggle-name)
                         (intern toggle-name))))
    `(progn
       (defun ,toggle-sym ()
         (list ,toggle-docstring)
         (interactive)
         (setq ,name (not ,name)))
       ,(if keybind `(map! ,keybind #',toggle-sym)))))

;;*** Toggle Variables

;;** Extract Data

;; TODO: add (... &key value)
(defun dc/find-symbols-like (regexp)
  (let (vars output)
    (cl-do-symbols (sym)
      (when (and
             (boundp sym)
             (not (fboundp sym))
             (s-matches? regexp (symbol-name sym)))
        (push (symbol-name sym) vars)))

    ;; how to use (with-temp-buffer) to accumulate?
    ;; (cl-loop for (sym) in (seq-filter (lambda (s) (s-matches? regexp (symbol-name s))) vars)
    ;;          ;; collect (concat (symbol-name sym) " \"" (symbol-value sym) "\"")
    ;;          collect (symbol-name sym)
    ;;          )
    vars))

(defun dc/eval-length-toggle-truncation ()
  (interactive)
  ;; monoid
  (if (not (boundp 'eval-length-toggle-truncation))
      (setq-local eval-length-toggle-truncation 'off
                  eval-expression-print-length-default eval-expression-print-length
                  print-length-default print-length))

  (cond ((eq eval-length-toggle-truncation 'on)
         (setq-local eval-length-toggle-truncation 'off
                     eval-expression-print-length eval-expression-print-length-default
                     print-length print-length-default))
        ((eq eval-length-toggle-truncation 'off)
         (setq-local eval-length-toggle-truncation 'on
                     eval-expression-print-length nil
                     print-length nil))))

(defun dc/eval-level-toggle-truncation ()
  (interactive)
  ;; monoid
  (if (not (boundp 'eval-level-toggle-truncation))
      (setq-local eval-level-toggle-truncation 'off
                  eval-expression-print-level-default eval-expression-print-level
                  print-level-default print-level))

  (cond ((eq eval-level-toggle-truncation 'on)
         (setq-local eval-level-toggle-truncation 'off
                     eval-expression-print-level eval-expression-print-level-default
                     print-level print-level-default))
        ((eq eval-level-toggle-truncation 'off)
         (setq-local eval-level-toggle-truncation 'on
                     eval-expression-print-level nil
                     print-level nil))))

;;** Libs

;;*** SRV

;; SRV implements RFC 2782 (SRV records).  It is used to look up hostname and port
;; for a service at a specific domain.  There might be multiple results, and the
;; caller is supposed to attempt to connect to each hostname+port in turn.
;; (setup (:pkg srv))

;;*** FSM
;; This package provides Finite State Machine library to make asynchronous
;; programming in Emacs Lisp easy and fun.
;; (setup (:pkg fsm))

(provide 'dc-support)

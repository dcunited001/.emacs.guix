;; -*- lexical-binding: t; -*-

;;* Support

;;** Macros

;; NOTE: if parsing the body to extract bindings is necessary,
;; use macroexp-parse-body
(defmacro dc/toggleable-boolean (name &optional keybind)
  "Define an interactive defun to toggle the variable NAME
along with KEYBIND, if present"
  (declare (intent defun))
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

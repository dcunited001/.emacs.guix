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

(provide 'dc-support)

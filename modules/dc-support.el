;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2023 David Conner
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;* Support

;;** No Littering Paths

(defalias 'dc/emacs-etc #'no-littering-expand-etc-file-name)
(defalias 'dc/emacs-var #'no-littering-expand-var-file-name)

;;** Doom

;; DOOM: ./lisp/core/doom-lib.el
(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

;;** ELD

;; projectile already has unserialize/deserialize to/from ELD
(require 'projectile)
(defalias 'dc/eld-serialize 'projectile-serialize)
(defalias 'dc/eld-unserialize 'projectile-unserialize)

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
(require 'fsm)

(provide 'dc-support)

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

;;** Doom

;; DOOM: ./lisp/core/doom-lib.el
(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

;;** Editor

;;*** Advice

;;**** Toggle Advice

;; TODO: parameterize bufler-switch-advice with: (f place before)
;; as: dc/toggle-advised-command and dc/toggle-advised-function

(defun dc/toggle-bufler-switch-advice ()
  "Toggle the `ace-window' advice function on `bufler-switch-buffer'"
  (interactive)
  (let* ((f 'bufler-switch-buffer)
         ;; unused
         (sf (symbol-function f))
         (place :before)
         (adf #'ace-select-window)
         (is-advice (advice--p (advice--symbol-function f))))
    (if is-advice
        ;; is advice should be (advice oclosure), since it's interactive
        (advice-remove f adf)
      (advice-add f place adf))))

(defun dc/toggle-advised-command (fsym place fn)
  "Toggle an advised function `fn' on function symbol `fsym'."
  ;; TODO: extend with keywords to more fully mirror the advice-add spec
  (interactive)
  ;; check args?
  (let* ((is-advice (advice--p (advice--symbol-function fsym))))
    (if is-advice
        ;; is advice should be (advice oclosure), since it's interactive
        (advice-remove fsym fn)
      (advice-add fsym place fn))))

;; doesn't quite work when focus is removed from help-buffer
;; (dc/toggle-advised-command 'help-view-source :before #'ace-select-window)

;;*** Macros

;;*** When Exec Found

(defun dc/exec-found? (cmd &optional remote sym)
  "Check for the presence of formatter command using exectable-find,
preferring the value of sym if present"
  (let ((cmd (or (bound-and-true-p sym) cmd)))
    (executable-find cmd remote)))

(defmacro dc/when-exec-found (formatter-cmd &optional formatter-sym &rest body)
  "Return lambda that checks for an executable. Warn if executable
doesn't exist."
  `(lambda ()
     (let ((remote? (file-remote-p default-directory)))
       (if-let* ((formatter-loc (dc/exec-found? ,formatter-cmd
                                                remote?
                                                ,formatter-sym)))
           ;; (progn
           ;;   (warn "%s found" formatter-loc)
           ;;   ,@body)
           ,@body
         (warn "Could not find %s (remote: %s)" ,formatter-cmd remote?)))))

;;** Extract Data

;; =============================================
;; a handy regexp from lisp-mode-el (via rx.el, also in use-package.el)
;;
;; [[file:./lisp/emacs-lisp/lisp-mode.el.gz::rx-define lisp-mode-symbol
;; [[file:./lisp/emacs-lisp/rx.el.gz::defun rx--translate-syntax
;; ---------------------------------------------
;; (rx-define lisp-mode-symbol (+ (| (syntax word)
;;                                   (syntax symbol)
;;                                   (: "\\" nonl))))
;; or just: lisp-mode-symbol-regexp
;;
;; (rx lisp-mode-symbol) ;; => "\\(?:\\sw\\|\\s_\\|\\\\.\\)+"
;; ---------------------------------------------

;; (lisp-data-mode-syntax-table) ;; hashtable with junk
;; but it gets increasingly sophisticated. it read simple lisp

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
;; (require 'fsm)

(provide 'dc-support)

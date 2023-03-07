;; -*- lexical-binding: t; -*-
;;; dc-popup.el --- Description
;;
;; Copyright © 2023 David Conner
;; Copyright © 2014-2022 Henrik Lissner.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;;; Commentary:
;;
;;; Code:

;;* Popups

(require 'a)

;; popup code is mostly self-contained
;; popup/config.el can't be included bc it calls (load! "hacks.el")
;; popup/autoloads/settings.el:
;; - 3 defuns and 2 defvar
;; - needed for +popup-defaults
;; - no external deps (refereces to +symbols)
;; popup/autoloads/popup.el:
;; - all defuns, except +popup--internal
;; - many functions needed
;; - no external deps (refereces to +symbols)
(defun dc/load-doom-popup ()
  (let ((popup-files '("ui/popup/autoload/settings.el"
                       "ui/popup/autoload/popup.el")))
    (dolist (doom-file popup-files)
      (load-file (expand-file-name doom-file dc/emacs-doom-modules)))))

(dc/load-doom-popup)

;; doom loads autoload definitions before ./modules/ui/popup/config.el
(require 'doom-popup-config)
(require 'dc-doom-popup-rules)

;;** Popper

(setup (:pkg popper
             :straight t
             :host github
             :repo "karthink/popper"
             :build (:not autoloads))

  (:option popper-display-control nil   ;t or 'user
           popper-window-height 25
           popper-reference-buffers '(eshell-mode
                                      vterm-mode
                                      geiser-repl-mode
                                      help-mode
                                      grep-mode
                                      helpful-mode
                                      compilation-mode
                                      elfeed-mode
                                      "^\\*Guix"))

  ;; popper-display-function matches display-buffer's action interface
  ;; - only affects popups where matching display-buffer-alist
  ;; - only called when popper-display-control is non-nil
  (:option popper-display-function #'popper-display-popup-at-bottom)
  (require 'popper)
  (popper-mode 1))

(defun popper-display-popup-at-top (buffer &optional alist)
  "Display popup-buffer BUFFER at the bottom of the screen."
  (display-buffer-in-side-window
   buffer
   (append alist
           `((window-height . ,popper-window-height)
             (side . top)
             (slot . 1)))))

(defun popper-select-popup-at-top (buffer &optional alist)
  "Display and switch to popup-buffer BUFFER at the bottom of the screen."
  (let ((window (popper-display-popup-at-top buffer alist)))
    (select-window window)))

(setq popper-display-function #'popper-select-popup-at-top)

;;** Config

(defun dc/+set-popup-rule (predicate &rest plist)
  (push (+popup-make-rule predicate plist) +popup--display-buffer-alist)
  ;; (when (not popper-display-control) ;; (bound-and-true-p +popup-mode)
  ;;   (setq display-buffer-alist +popup--display-buffer-alist))
  (setq display-buffer-alist +popup--display-buffer-alist)
  +popup--display-buffer-alist)

(defun dc/+set-popup-rules (&rest rulesets)
  (dolist (rules rulesets)
    (dolist (rule rules)
      (push (+popup-make-rule (car rule) (cdr rule))
            +popup--display-buffer-alist)))
  ;; (when (not popper-display-control) ;; (bound-and-true-p +popup-mode)
  ;;   (setq display-buffer-alist +popup--display-buffer-alist))
  (setq display-buffer-alist +popup--display-buffer-alist)
  +popup--display-buffer-alist)

;;** Popup Configuration Management

(defconst dc/popper-display-buffer-alist-defaults
  '((popper-display-control-p (popper-select-popup-at-bottom))))

(defun dc/popup-rules-clear ()
  (interactive)
  (setq display-buffer-alist dc/popper-display-buffer-alist-defaults
        +popup--display-buffer-alist dc/popper-display-buffer-alist-defaults))

(defun dc/popup-use-popper ()
  "Clear popup rules and use popper's config to manage popups."
  (interactive)
  ;; (setq popper-display-control t)
  (dc/popup-rules-clear))

(defvar dc/doom-popup-rules-selected-rulesets
  (a-keys dc/doom-popup-rules))

(defun dc/popup-rules-reset (&optional selected)
  "Reset popups to a determinate state. Clears rules from
display-buffer-alist. Then, sets display-buffer-alist to the
selected rulesets. SELECTED is a list of keys"
  (interactive)
  (dc/popup-rules-clear)
  ;; (setq popper-display-control nil)
  (dc/popup-rules-set dc/doom-popup-rules))

(defun dc/popup-rulesets-set (&optional rulesets selected)
  "Sets display-buffer-alist to the selected rulesets. SELECTED is a
list of keys"
  (interactive)

  (let* ((rulesets (or rulesets dc/doom-popup-rules))
         (default-keys dc/doom-popup-rules-selected-rulesets)
         (selected-keys (or selected default-keys))
         (selected-rules (list (dc/popup-rulesets-select rulesets selected-keys))))
    (apply #'dc/+set-popup-rules selected-rules)))

(defun dc/popup-rulesets-select (rulesets &optional selected)
  "Collect dc/doom-popup-rules into a list of alists. If SELECTED is
a list of keys, limit the selection to those keys. Only the
interactive methods will default to selecting all keys."
  (cl-reduce (lambda (acc k)
               (a-merge acc (a-get rulesets k)))
             selected
             :initial-value (a-list)))

;; (dc/popup-rulesets-select dc/doom-popup-rules '(all clojure))
;; (dc/popup-rulesets-set dc/doom-popup-rules '(customize))
;; (dc/popup-rulesets-set)

(provide 'dc-popup)

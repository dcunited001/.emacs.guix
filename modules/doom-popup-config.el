;; -*- lexical-binding: t; -*-
;;; doom-popup-config.el --- Description
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

(defconst +popup-window-parameters '(ttl quit select modeline popup)
  "A list of custom parameters to be added to `window-persistent-parameters'.
Modifying this has no effect, unless done before ui/popup loads.")

(defvar +popup-default-display-buffer-actions
  '(+popup-display-buffer-stacked-side-window-fn)
  "The functions to use to display the popup buffer.")

(defvar +popup-default-alist
  '((window-height . 0.16) ; remove later
    (reusable-frames . visible))
  "The default alist for `display-buffer-alist' rules.")

(defvar +popup-default-parameters
  '((transient . t)   ; remove later
    (quit . t)        ; remove later
    (select . ignore) ; remove later
    (no-other-window . t))
  "The default window parameters.")

(defvar +popup-margin-width 1
  "Size of the margins to give popup windows. Set this to nil to disable margin
adjustment.")

(defvar +popup--inhibit-transient nil)
(defvar +popup--inhibit-select nil)
(defvar +popup--old-display-buffer-alist nil)
(defvar +popup--remember-last t)
(defvar +popup--last nil)
(defvar-local +popup--timer nil)

(define-minor-mode +popup-buffer-mode
  "Minor mode for individual popup windows.

It is enabled when a buffer is displayed in a popup window and disabled when
that window has been changed or closed."
  :init-value nil
  ;; :keymap +popup-buffer-mode-map
  (if (not +popup-buffer-mode)
      (remove-hook 'after-change-major-mode-hook #'+popup-set-modeline-on-enable-h t)
    (add-hook 'after-change-major-mode-hook #'+popup-set-modeline-on-enable-h
              nil 'local)
    (when (timerp +popup--timer)
      (remove-hook 'kill-buffer-hook #'+popup-kill-buffer-hook-h t)
      (cancel-timer +popup--timer)
      (setq +popup--timer nil))))

(put '+popup-buffer-mode 'permanent-local t)
(put '+popup-buffer-mode 'permanent-local-hook t)
(put '+popup-set-modeline-on-enable-h 'permanent-local-hook t)

(defmacro with-popup-rules! (rules &rest body)
  "Evaluate BODY with popup RULES. RULES is a list of popup rules. Each rule
should match the arguments of `+popup-define' or the :popup setting."
  (declare (indent defun))
  `(let ((+popup--display-buffer-alist +popup--old-display-buffer-alist)
         display-buffer-alist)
     (set-popup-rules! ,rules)
     ;; (when (bound-and-true-p +popup-mode)
     ;;   (setq display-buffer-alist +popup--display-buffer-alist))
     (setq display-buffer-alist +popup--display-buffer-alist)
     ,@body))

(provide 'doom-popup-config)

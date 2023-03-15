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

;;* Mouse
;; NOTE: there are basically no mouse-maps using:
;; - super
;; - shift with any other modkey

;; see stroke-mode for examples of complex mouse stuff
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/strokes.el

(require 'a)
(require 'dash)

;;** TTY
(add-hook 'tty-setup-hook #'xterm-mouse-mode) ;doom

;; Fix an issue where these two variables aren't defined in TTY Emacs on MacOS
(defvar mouse-wheel-down-event nil)
(defvar mouse-wheel-up-event nil)

(defvar mousemaps-unbound-at-init
  ;; mouse-1
  '("C-M-<mouse-1>"  ;mouse-set-point
    ;; "C-M-<down-mouse-1>" ;ok w/ mouse-drag-region-rectangle?
    "M-<mouse-1>"                       ;mouse-start-secondary
    "M-<drag-mouse-1>"                  ;mouse-drag-secondary
    "M-<down-mouse-1>"                  ;mouse-set-secondary

    ;; mouse-2 (for me, the wheel button, a little awkward)
    "<mouse-2>"                         ;mouse-yank-primary
    "M-<mouse-2>"                       ;yank-secondary

    ;; mouse-3
    "<mouse-3>"                         ;mouse-save-then-kill
    "S-<mouse-3>"                       ;kmacro-end-call-mouse
    "M-<mouse-3>"                       ;mouse-secondary-save-then-kill
    "C-<mouse-3>"
    "<drag-mouse-3>"                    ;mouse-secondary-yank
    "<mode-line> <mouse-3>"             ;mouse-delete-window

    "S-<wheel-up>"                       ;mwheel-scroll
    "S-<wheel-down>"                     ;mwheel-scroll

    ))

;; this variable doesn't do anything on either computer
;; (setq mouse-wheel-follow-mouse nil)
;; because (window-live-p window) is called on nil window
;; ((window (if mouse-wheel-follow-mouse
;;              (mwheel-event-window event)
;;            (selected-window)))
;;  (frame (when (window-live-p window)
;;           (frame-parameter
;;            (window-frame window) 'mouse-wheel-frame))))
(defun mouse-wheel-scroll-active-window (event &optional arg)
  (interactive (list last-input-event current-prefix-arg))
  (let ((mouse-wheel-follow-mouse nil))
    (mwheel-scroll event arg)))

(general-unbind :keymaps 'global "M-<wheel-down>" "M-<wheel-up>")
(general-define-key
 :keymaps 'global
 "M-<wheel-down>" #'mouse-wheel-debug-event
 "M-<wheel-up>" #'mouse-wheel-scroll-active-window
 ;; "M-<wheel-down>" #'mouse-wheel-scroll-active-window
 ;; "M-<wheel-up>" #'mouse-wheel-scroll-active-window
 )

;; (defun mouse-wheel-scroll-minibuffer (event)
;;   "Rebind mouse EVENT to the currently active minibuffer."
;;   (interactive (list last-input-event))
;;   (if-let ((selected-window (minibuffer-window))
;;            ;; NOTE see mouse-wheel--get-scroll-window
;;            (scroll-window (mouse-wheel--get-scroll-window event))
;;            (button (mwheel-event-button event)))
;;       (progn
;;         (select-window scroll-window 'mark-for-redisplay)
;;         ())
;;     (user-error "Minibuffer probably not active")))

;; hmmm how to refactor?
(defun dc/mousemaps-rebind-mouse-split ()
  (let* ((mouse-contexts
          '(("<right-divider>" . "mouse-split-window-vertically")
            ("<vertical-line>" . "mouse-split-window-vertically")
            ("<vertical-scroll-bar>" . "mouse-split-window-vertically")
            ("<bottom-divider>" . "mouse-split-window-horizontally")
            ("<mode-line>" . "mouse-split-window-horizontally")
            ("<horizontal-scroll-bar>" . "mouse-split-window-horizontally")))
         (mouse-unbinds
          (->> (a-keys mouse-contexts)
               ;; #'apply-partially and not #'(apply-partially ...)?
               ;; (-map (funcall #'apply-partially 'concat "C-"))
               (-map (funcall #'apply-partially 's-append " C-<mouse-2>"))
               ))
         (mouse-rebinds
          (a-reduce-kv
           (lambda (acc context fn-str)
             ;; to push or to add-to-list?
             (push (symbol-function (intern fn-str)) acc)
             (push (concat context " C-<mouse-3>") acc))
           ;; (a-merge acc (a-list (concat context " C-<mouse-3>")
           ;;                      (symbol-function (intern fn-str))))
           nil mouse-contexts)))

    (cl-dolist (kbd mouse-unbinds) (unbind-key kbd))
    (apply #'general-define-key :keymaps 'global mouse-rebinds)))

(unbind-key "<mode-line> <mouse-2>")  ;mouse-delete-other-windows
(unbind-key "<mode-line> <mouse-3>")  ;mouse-delete-window
(dc/mousemaps-rebind-mouse-split)

;; TODO: get the code below to work with two-finger gestures on laptop
;; '((#'select-frame-by-name . ("s-<mouse-9>" "s-<wheel-right>")))

(general-define-key
 :keymap 'global
 "s-<mouse-9>" #'select-frame-by-name
 "S-<mouse-8>" #'previous-window-any-frame
 "S-<mouse-8>" #'next-window-any-frame

 ;; TODO xref-goto-xref (interactive "P" is weird)
 ;; TODO defmacro: mousify functions with (interactive "e")
 ;; - useful if decorating a caller to a cl-defgeneric/method
 ;; TODO search functions decorated by (interactive "e")
 ;; TODO wrap xref-go-back/forward to call alternative fn when no xref stack
 "<mouse-8>" #'xref-go-back
 "<mouse-9>" #'xref-go-forward
 "M-<mouse-8>" #'xref-find-definitions-at-mouse
 "M-<mouse-9>" #'xref-find-references-at-mouse

 ;; NOTE: if i set this, i will blow my xref-stack
 ;; "<mouse-8>" #'previous-buffer

 ;; scrolling to things registered in imenu would be better
 ;; TODO: get this to use treesitter if it's available in a mode
 ;; - the combination of touchpad drivers and <triple-wheel-up/down>
 ;;   would be a little crazy
 "S-<wheel-down>" #'forward-paragraph
 "S-<wheel-up>" #'backward-paragraph)

(provide 'dc-mouse)

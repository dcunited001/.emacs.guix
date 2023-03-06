;; -*- lexical-binding: t; -*-
;;; dc-doom-popup.el --- Description
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
;; Borrows heavily if not literally from Doom Emacs' +popup modules
;;
;; Not sure how to attribute copyrights for dotfiles/config.  It wouldn't
;; usually qualify as fair use.  When including software by package/module then
;; attributing is simple, I feel like if you solve the piecewise dotfiles/config
;; reference problem -- without being overwhelmed by mileau -- then your SBOM or
;; software supply chain solution could probably survive anything.
;;
;; These are dotfiles.  I'm configuring a personal system to behave according to
;; preferable logic.  Whenever I get to actually fucking programming, that's
;; different.
;;
;; Ultimately, any sufficiently advanced autocomplete cannot be distinguished
;; from Github Co-Pilot.  If you don't understand how much you depend on it, try
;; going without it and see how much "original" code you produce.
;;
;; From the perspective of cybernetics, autocomplete is just a signal inducing
;; you to create content.  The magnitude of influence lies along a spectrum.  If
;; your content is not identical without autocomplete or is not produced with
;; the same rhythm/style, then you are affected by that signal.  Whether VSCode
;; with LSP on corporate docker or dabbrev or Github co-pilot: this is simply a
;; change in cybernetic signal source, albeit qualitatively different in various
;; modes; ethical consequence being one such mode.  If the context is music and
;; the cybernetic signal source is a bassline, the jass soloist is being
;; modulated by the signal and would prefer better inspiration.  There are
;; obviously qualities in the music produced compared with the source signal
;; that would indicate originality and novelty.
;;
;; How is signal modulation distintuished from correlation? Where is the signal
;; modulation complexity burden found with completion systems/interfaces? In the
;; user processing a popup with docs? Or in the asynchronous background tasks:
;; rtags, *tags, LSP, eglot? When someone doesn't independantly modulate a
;; signal, it modulates them.  In the extreme, this is the inversion of Kantian
;; transcendentalism that causes the machinic to construct the human's interface
;; to the unconscious.
;;
;; As for computer science, if you are good, then your code (judged as a work of
;; computer science) may be original, but it is never novel.  See Donald Knuth's
;; series...
;;
;; Goddamit I philosophied again.  It's really just a particular kind of
;; ego-gynmastics.
;;
;;; Code:

(require 'dc-doom-popup-rules)
(setup (:pkg a))

;;* Popups

;;** Popper

(setup (:pkg popper
             :straight t
             :host github
             :repo "karthink/popper"
             :build (:not autoloads))
  (:option popper-window-height 20
           ;; popper-window-height
           ;; (lambda (window)
           ;;   (let ((buffer-mode (with-current-buffer (window-buffer window)
           ;;                        major-mode)))
           ;;     (message "BUFFER MODE: %s" buffer-mode)
           ;;     (pcase buffer-mode
           ;;       ('exwm-mode 40)
           ;;       ('helpful-mode 20)
           ;;       ('eshell-mode (progn (message "eshell!") 10))
           ;;       (_ 15))))
           popper-reference-buffers '(eshell-mode
                                      vterm-mode
                                      geiser-repl-mode
                                      help-mode
                                      grep-mode
                                      helpful-mode
                                      compilation-mode
                                      elfeed-mode
                                      "^\\*Guix"))
  (require 'popper) ;; Needed because I disabled autoloads
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

;; not sure how to get popups on the side
;; - oh well

;;** Doom Popups

;; shamelessly copy
(defun +popup-shink-to-fit (&optional window)
  "Shrinks WINDOW to fit the buffer contents, if the buffer isn't empty."
  (unless window (setq window (selected-window)))
  (unless (= (- (point-max) (point-min)) 0)
    (shrink-window-if-larger-than-buffer window)))

(defvar +popup--display-buffer-alist nil)

(defvar +popup-defaults
  (list :side 'bottom
        :height 0.16
        :width 40
        :quit t
        :select #'ignore
        :ttl 5))

(defun +popup-make-rule (predicate plist)
  (if (plist-get plist :ignore)
      (list predicate nil)
    (let* ((plist (append plist +popup-defaults))
           (alist
            `((actions . ,(plist-get plist :actions))
              (side . ,(plist-get plist :side))
              (size . ,(plist-get plist :size))
              (window-width . ,(plist-get plist :width))
              (window-height . ,(plist-get plist :height))
              (slot . ,(plist-get plist :slot))
              (vslot . ,(plist-get plist :vslot))))
           (params
            `((ttl . ,(plist-get plist :ttl))
              (quit . ,(plist-get plist :quit))
              (select . ,(plist-get plist :select))
              (modeline . ,(plist-get plist :modeline))
              (autosave . ,(plist-get plist :autosave))
              ,@(plist-get plist :parameters))))
      `(,predicate ,@alist
                   (window-parameters ,@params)))))

(defun +popup-parameter (parameter &optional window)
  "Fetch the window PARAMETER (symbol) of WINDOW"
  (window-paramter (or window (selected-window)) parameter))

(defun +popup-parameter-fn (parameter &optional window &rest args)
  "Fetch the window PARAMETER (symbol) of WINDOW. If it is a
function, run it with ARGS to get its return value."
  (let ((val (+popup-parameter parameter window)))
    (if (functionp val)
        (apply val args)
      val)))

(defun dc/+set-popup-rule (predicate &rest plist)
  (push (+popup-make-rule predicate plist) +popup--display-buffer-alist)
  (when (not popper-display-control) ;; (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist))
  +popup--display-buffer-alist)

(defun dc/+set-popup-rules (&rest rulesets)
  (dolist (rules rulesets)
    (dolist (rule rules)
      (push (+popup-make-rule (car rule) (cdr rule))
            +popup--display-buffer-alist)))
  (when (not popper-display-control) ;; (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist))
  +popup--display-buffer-alist)

;;** Popup Configuration Management

(defconst dc/popper-display-buffer-alist-defaults
  '((popper-display-control-p (popper-select-popup-at-bottom))))

(defun dc/popup-rules-clear ()
  (setq display-buffer-alist dc/popper-display-buffer-alist-defaults
        +popup--display-buffer-alist dc/popper-display-buffer-alist-defaults))

(defvar dc/doom-popup-rules-selected-rulesets
  (a-keys dc/doom-popup-rules))

(defun dc/popup-rules-set (&optional selected)
  "Reset popups to a determinate state. Clears rules from
display-buffer-alist. Then, sets display-buffer-alist to the
selected rulesets. SELECTED is a list of keys"
  (interactive)
  (dc/popup-rules-clear)

  (let ((selected-rulesets
         (or selected-rulesets
             dc/doom-popup-rules-selected-rulesets))
        (selected-rules (dc/popup-rules-select selected-rulesets)))

    ))

(defun dc/popup-rules-select (&optional selected)
  "Collect dc/doom-popup-rules into a list of alists. If SELECTED is
a list of keys, limit the selection to those keys. Only the
interactive methods will default to selecting all keys."

  ;; allow override of defaults with a limited set of keys

  (a-reduce-kv (lambda (acc k v)
                 (a-merge acc (a-get dc/doom-popup-rules k)))
               (a-list)
               dc/doom-popup-rules
               ;; (a-keys dc/doom-popup-rules)
               ))

;; (setq popper-display-control nil)

display-buffer-alist

(a-get display-buffer-alist "^\\*Customize")


(provide 'dc-doom-popup)

;; -*- lexical-binding: t; -*-
;;; dc-doom-popup-rules.el --- Description
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
;; No comment
;;
;;; Code:

(require 'a)
(defvar dc/doom-popup-rules-defaults
  ;; handles all special buffers
  '((starred
     ("^\\*"  :slot 1 :vslot -1 :select t)
     ("^ \\*" :slot 1 :vslot -1 :size +popup-shrink-to-fit))
    (compilation
     ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\)"
      :vslot -2 :size 0.3  :autosave t :quit t :ttl nil))
    (messages
     ("^\\*\\(?:Messages\\)"
      :vslot -2 :size 0.3  :autosave t :quit t :ttl nil))

    ;; doom buffers

    ;; transient buffers (no interaction required)
    (doom-buffers-no-interaction
     ("^\\*\\(?:doom \\|Pp E\\)"
      :vslot -3 :size +popup-shrink-to-fit :autosave t
      :select ignore :quit t :ttl 0))
    ;; editing buffers (interaction-required
    (doom-buffers-with-interaction
     ("^\\*doom:.*-popup"
      :vslot -4 :size 0.35 :select t :modeline nil :quit nil :ttl nil))

    ;; shell

    (vterm
     ("^\\*\\(?:vterm\\)"
      :vslot -4 :size 0.35 :select t :modeline nil :quit nil :ttl nil))
    (eshell
     ("^\\*\\(?:eshell\\)"
      :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil))
    (direnv
     ("^\\*envrc\\*" :quit t :ttl 0))

    ;; misc

    (woman
     ("^\\*\\(?:Wo\\)?Man "
      :vslot -6 :size 0.45 :select t :quit t :ttl 0))
    (calc
     ("^\\*Calc"
      :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0))

    ;; UI

    (customize
     ("^\\*Customize"
      :side left :vslot -2 :slot 2 :size 0.35 :select t :quit nil))
    (undo-tree
     ("^ \\*undo-tree\\*"
      :side left :vslot -4 :slot 2 :size 20 :select t :quit t))
    (embark
     ("^ \\*Embark Live"
      :side Bottom :vslot -4 :size 0.4 :select t :quit t))

    ;; help

    ;; `help-mode', `helpful-mode'
    (help
     ("^\\*\\([Hh]elp\\|Apropos\\)"
      :slot 2 :vslot -8 :size 0.42 :select t))
    ;; `eww' (and used by dash docsets)
    (eww ("^\\*eww\\*" :vslot -11 :size 0.35 :select t))
    (xwidget ("^\\*xwidget" :vslot -11 :size 0.35 :select nil))
    ;; Info-mode
    (info ("^\\*info\\*$" :slot 2 :vslot 2 :size 0.45 :select t))

    ;; system (vslot 99-101)

    (warnings ("^\\*Warnings" :vslot 99 :size 0.25))
    (backtrace ("^\\*Backtrace" :vslot 99 :size 0.4 :quit nil))
    (profiler-reports
     ("^\\*CPU-Profiler-Report "
      :side bottom :vslot 100 :slot 1 :height 0.4 :width 0.5 :quit nil)
     ("^\\*Memory-Profiler-Report "
      :side bottom :vslot 100 :slot 2 :height 0.4 :width 0.5 :quit nil))
    (process-list
     ("^\\*Process List\\*"
      :side bottom :vslot 101 :size 0.25 :select t :quit t))
    (ignore
     ("^\\*\\(?:Proced\\|timer-list\\|Abbrevs\\|Output\\)\\*" :ignore t)
     ("^\\*\\(?:Occur\\|unsent mail.*?\\|message\\)\\*" :ignore t))

    ;; tools (flycheck/vc/pdf, no slots)

    (flycheck
     ("^\\*Flycheck error messages\\*" :select nil)
     ("^\\*Flycheck errors\\*" :size 0.25))
    (vc
     ("^\\*vc-diff" :select nil)
     ("^\\*vc-change" :select t))
    (pdf
     ("^\\*Outline*" :side right :size 40 :select nil)
     ("^\\*Edit Annotation " :quit nil)
     ("\\(?:^\\*Contents\\|'s annots\\*$\\)" :ignore t))

    ;; repls

    (geiser
     ("^\\*[gG]eiser \\(dbg\\|xref\\|messages\\)\\*$" :slot 1 :vslot -1)
     ("^\\*Geiser documentation\\*$" :slot 2 :vslot 2 :select t :size 0.35)
     ("^\\* [A-Za-z0-9_-]+ REPL \\*" :size 0.3 :quit nil :ttl nil))
    (common-lisp
     ("^\\*sly-mrepl" :vslot 2 :size 0.3 :quit nil :ttl nil)
     ("^\\*sly-compilation" :vslot 3 :ttl nil)
     ("^\\*sly-traces" :vslot 4 :ttl nil)
     ("^\\*sly-description" :vslot 5 :size 0.3 :ttl 0)
     ;; Do not display debugger or inspector buffers in a popup
     ;; window. These buffers are meant to be displayed with sufficient
     ;; vertical space.
     ("^\\*sly-\\(?:db\\|inspector\\)" :ignore t))
    (clojure
     ("^\\*cider-error*" :ignore t)
     ("^\\*cider-repl" :quit nil :ttl nil)
     ("^\\*cider-repl-history" :vslot 2 :ttl nil))

    ;; documents

    (org
     ("^\\*Org Links" :slot -1 :vslot -1 :size 2 :ttl 0)
     ("^ ?\\*\\(?:Agenda Com\\|Calendar\\|Org Export Dispatcher\\)"
      :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :ttl 0)
     ("^\\*Org \\(?:Select\\|Attach\\)" :slot -1 :vslot -2 :ttl 0 :size 0.25)
     ("^\\*Org Agenda" :ignore t)
     ("^\\*Org Src" :size 0.42 :quit nil :select t :autosave t :modeline t :ttl nil)
     ("^\\*Org-Babel")
     ("^\\*Capture\\*$\\|CAPTURE-.*$" :size 0.42 :quit nil :select t :autosave ignore))
    (org-roam
     `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
        :side right :width 0.33 :height 0.5 :ttl nil :modeline nil :quit nil :slot 1)
       ("^\\*org-roam: "                ; node dedicated org-roam buffer
        :side right :width 0.33 :height 0.5 :ttl nil :modeline nil :quit nil :slot 2)))
    (ipython
     ("\\*ob-ipython.*"
      :slot 2 :side right :size 100 :height 0.2
      :select nil :quit nil :ttl nil)
     ("^ \\*Python"
      :slot 0 :side right :size 100
      :select nil :quit nil :ttl nil))
    (latex
     (" output\\*$" :size 15)
     ("^\\*TeX \\(?:Help\\|errors\\)" :size 0.3 :select t :ttl nil))
    (ledger
     ("^\\*Ledger Report" :size 0.5 :quit 'other :ttl 0)
     ("^\\*Ledger Error"  :quit t :ttl 0))))


;; TODO: deep-merge?
(defvar dc/doom-popup-rules-custom
  '((xref
     ("^\\*xref\\*"
      :side top :vslot -1 :slot 3 :size 0.20 :select t :quit t))
    (help
     ("^\\*\\([Hh]elp\\|Apropos\\)"
      :side top :vslot -1 :slot 2 :size 0.42 :select t))
    (flycheck
     ("^\\*Flycheck error messages\\*" :select nil)
     ("^\\*Flycheck errors\\*"
      :side top :vslot -2 :slot 1 :size 0.25))

    ;; lsp-ui-menui shows up on the right when set to left
    ;; here and in doom emacs

    (lsp-ui-menu
     ("^\\*lsp-ui-imenu"
      :side right :vslot -5 :width 60 :select t :quit t))
    (bufler
     ("^\\*Bufler"
      :side right :vslot -5 :slot -5 :width 80 :select t :quit t))

    ;; elfeed doesn't seem to work
    (elfeed
     ("^\\*elfeed-entry" ;; :actions '(display-buffer-below-selected)
      :modeline nil :select t :quit t))))

(defun dc/doom-popup-rules-init ()
  (setq dc/popup-rules
        (a-merge dc/doom-popup-rules-defaults dc/doom-popup-rules-custom)))
(dc/doom-popup-rules-init)

(provide 'dc-doom-popup-rules)
;;; dc-doom-popup-rules.el ends here

;; (with-popup-rules! dc/popup-rules)

;; (let ((rules (list (a-get dc/doom-popup-rules-custom 'help))))
;;   (with-popup-rules!
;;     rules
;;     (apropos-command "display")))

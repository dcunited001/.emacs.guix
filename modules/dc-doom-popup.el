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

(setup (:pkg a))

;; shamelessly copy
(defun +popup-shink-to-fit (&optional window)
  "Shrinks WINDOW to fit the buffer contents, if the buffer isn't empty."
  (unless window (setq window (selected-window)))
  (unless (= (- (point-max) (point-min)) 0)
    (shrink-window-if-larger-than-buffer window)))

(setq dc/doom-popup-rules
      ;; handles all special buffers
      '((all
         (("^\\*"  :slot 1 :vslot -1 :select t)
          ("^ \\*" :slot 1 :vslot -1 :size +popup-shrink-to-fit)))
        (compilation
         (("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\)"
           :vslot -2 :size 0.3  :autosave t :quit t :ttl nil)))
        (messages
         (("^\\*\\(?:Messages\\)"
           :vslot -2 :size 0.3  :autosave t :quit t :ttl nil)))
        ;; transient buffers (no interaction required)
        (doom-buffers-no-interaction
         (("^\\*\\(?:doom \\|Pp E\\)"
           :vslot -3 :size +popup-shrink-to-fit :autosave t
           :select ignore :quit t :ttl 0)))
        ;; editing buffers (interaction-required
        (doom-buffers-with-interaction
         (("^\\*doom:.*-popup"
           :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)))
        ;; TODO: adjust vterm
        (vterm
         (("^\\*\\(?:vterm\\)"
           :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)))
        ;; TODO: adjust eshell
        (eshell
         (("^\\*\\(?:eshell\\)"
           :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)))
        (woman
         (("^\\*\\(?:Wo\\)?Man "
           :vslot -6 :size 0.45 :select t :quit t :ttl 0)))
        (calc
         (("^\\*Calc"
           :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0)))
        (customize
         (("^\\*Customize"
           :slot 2 :side right :size 0.5 :select t :quit nil)))
        (undo-tree
         (("^ \\*undo-tree\\*"
           :slot 2 :side left :size 20 :select t :quit t)))
        ;; `help-mode', `helpful-mode'
        (help
         (("^\\*\\([Hh]elp\\|Apropos\\)"
           :slot 2 :vslot -8 :size 0.42 :select t)))
        ;; `eww' (and used by dash docsets)
        (eww (("^\\*eww\\*" :vslot -11 :size 0.35 :select t)))
        (xwidget (("^\\*xwidget" :vslot -11 :size 0.35 :select nil)))
        ;; Info-mode
        (info (("^\\*info\\*$" :slot 2 :vslot 2 :size 0.45 :select t)))
        (warnings (("^\\*Warnings" :vslot 99 :size 0.25)))
        (backtrace (("^\\*Backtrace" :vslot 99 :size 0.4 :quit nil)))
        (profiler-reports
         (("^\\*CPU-Profiler-Report "
           :side bottom :vslot 100 :slot 1 :height 0.4 :width 0.5 :quit nil)
          ("^\\*Memory-Profiler-Report "
           :side bottom :vslot 100 :slot 2 :height 0.4 :width 0.5 :quit nil)))
        (process-list
         (("^\\*Process List\\*"
           :side bottom :vslot 101 :size 0.25 :select t :quit t)))
        (ignore
         (("^\\*\\(?:Proced\\|timer-list\\|Abbrevs\\|Output\\)\\*" :ignore t)
          ("^\\*\\(?:Occur\\|unsent mail.*?\\|message\\)\\*" :ignore t)))
        (flycheck
         (("^\\*Flycheck error messages\\*" :select nil)
          ("^\\*Flycheck errors\\*" :size 0.25)))
        (vc
         (("^\\*vc-diff" :select nil)
          ("^\\*vc-change" :select t)))
        (pdf
         (("^\\*Outline*" :side right :size 40 :select nil)
          ("^\\*Edit Annotation " :quit nil)
          ("\\(?:^\\*Contents\\|'s annots\\*$\\)" :ignore t)))
        (scheme
         (("^\\*[gG]eiser \\(dbg\\|xref\\|messages\\)\\*$" :slot 1 :vslot -1)
          ("^\\*Geiser documentation\\*$"
           :slot 2 :vslot 2 :select t :size 0.35)
          ("^\\* [A-Za-z0-9_-]+ REPL \\*" :size 0.3 :quit nil :ttl nil)))
        (common-lisp
         (("^\\*sly-mrepl" :vslot 2 :size 0.3 :quit nil :ttl nil)
          ("^\\*sly-compilation" :vslot 3 :ttl nil)
          ("^\\*sly-traces" :vslot 4 :ttl nil)
          ("^\\*sly-description" :vslot 5 :size 0.3 :ttl 0)
          ;; Do not display debugger or inspector buffers in a popup
          ;; window. These buffers are meant to be displayed with sufficient
          ;; vertical space.
          ("^\\*sly-\\(?:db\\|inspector\\)" :ignore t)))
        (clojure
         (("^\\*cider-error*" :ignore t)
          ("^\\*cider-repl" :quit nil :ttl nil)
          ("^\\*cider-repl-history" :vslot 2 :ttl nil)))
        (org
         (("^\\*Org Links" :slot -1 :vslot -1 :size 2 :ttl 0)
          ("^ ?\\*\\(?:Agenda Com\\|Calendar\\|Org Export Dispatcher\\)"
           :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :ttl 0)
          ("^\\*Org \\(?:Select\\|Attach\\)"
           :slot -1 :vslot -2 :ttl 0 :size 0.25)
          ("^\\*Org Agenda" :ignore t)
          ("^\\*Org Src"
           :size 0.42 :quit nil :select t :autosave t :modeline t :ttl nil)
          ("^\\*Org-Babel")
          ("^\\*Capture\\*$\\|CAPTURE-.*$"
           :size 0.42 :quit nil :select t :autosave ignore)))
        (ipython
         (("\\*ob-ipython.*"
           :slot 2 :side right :size 100 :height 0.2
           :select nil :quit nil :ttl nil)
          ("^ \\*Python"
           :slot 0 :side right :size 100
           :select nil :quit nil :ttl nil)))
        (latex
         ((" output\\*$" :size 15)
          ("^\\*TeX \\(?:Help\\|errors\\)" :size 0.3 :select t :ttl nil)))
        (ledger
         (("^\\*Ledger Report" :size 0.5 :quit 'other :ttl 0)
          ("^\\*Ledger Error"  :quit t :ttl 0)))
        (lsp
         (("^\\*lsp-ui-imenu" :side left :width 60
           :vslot -5 :slot 3
           :modeline nil :select t :quit t)))
        (help
         (("^\\*Help" :side left :width 60
           :vslot -5 :slot 1
           :modeline nil :select t :quit t)))
        (bufler
         (("^\\*Bufler" :side right :width 80
           :vslot -5 :slot -5
           :modeline nil :select t :quit t)))))

(provide 'dc-doom-popup)

;;* Doom

;;** Popup Config
;; (set-popup-rules!
;;  (when (modulep! +all)
;;    '(("^\\*"  :slot 1 :vslot -1 :select t)
;;      ("^ \\*" :slot 1 :vslot -1 :size +popup-shrink-to-fit)))
;;  (when (modulep! +defaults)
;;    '(("^\\*Completions" :ignore t)
;;      ("^\\*Local variables\\*$"
;;       :vslot -1 :slot 1 :size +popup-shrink-to-fit)
;;      ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
;;       :vslot -2 :size 0.3  :autosave t :quit t :ttl nil)
;;      ("^\\*\\(?:doom \\|Pp E\\)"   ; transient buffers (no interaction required)
;;       :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0)
;;      ("^\\*doom:"                       ; editing buffers (interaction required)
;;       :vslot -4 :size 0.35 :autosave t :select t :modeline t :quit nil :ttl t)
;;      ("^\\*doom:\\(?:v?term\\|e?shell\\)-popup" ; editing buffers (interaction required)
;;       :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)
;;      ("^\\*\\(?:Wo\\)?Man "
;;       :vslot -6 :size 0.45 :select t :quit t :ttl 0)
;;      ("^\\*Calc"
;;       :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0)
;;      ("^\\*Customize"
;;       :slot 2 :side right :size 0.5 :select t :quit nil)
;;      ("^ \\*undo-tree\\*"
;;       :slot 2 :side left :size 20 :select t :quit t)
;;      ;; `help-mode', `helpful-mode'
;;      ("^\\*\\([Hh]elp\\|Apropos\\)"
;;       :slot 2 :vslot -8 :size 0.42 :select t)
;;      ("^\\*eww\\*"                      ; `eww' (and used by dash docsets)
;;       :vslot -11 :size 0.35 :select t)
;;      ("^\\*xwidget"
;;       :vslot -11 :size 0.35 :select nil)
;;      ("^\\*info\\*$"                    ; `Info-mode'
;;       :slot 2 :vslot 2 :size 0.45 :select t)))
;;  '(("^\\*Warnings" :vslot 99 :size 0.25)
;;    ("^\\*Backtrace" :vslot 99 :size 0.4 :quit nil)
;;    ("^\\*CPU-Profiler-Report "    :side bottom :vslot 100 :slot 1 :height 0.4 :width 0.5 :quit nil)
;;    ("^\\*Memory-Profiler-Report " :side bottom :vslot 100 :slot 2 :height 0.4 :width 0.5 :quit nil)
;;    ("^\\*Process List\\*" :side bottom :vslot 101 :size 0.25 :select t :quit t)
;;    ("^\\*\\(?:Proced\\|timer-list\\|Abbrevs\\|Output\\|Occur\\|unsent mail.*?\\|message\\)\\*" :ignore t)))

;;** Flycheck
;; (set-popup-rules!
;;     '(("^\\*Flycheck error messages\\*" :select nil)
;;       ("^\\*Flycheck errors\\*" :size 0.25)))

;;** VC
;; (set-popup-rules!
;;     '(("^\\*vc-diff" :select nil)   ; *vc-diff*
;;       ("^\\*vc-change" :select t)))

;;** PDF
;; (set-popup-rules!
;;     '(("^\\*Outline*" :side right :size 40 :select nil)
;;       ("^\\*Edit Annotation " :quit nil)
;;       ("\\(?:^\\*Contents\\|'s annots\\*$\\)" :ignore t)))

;;** Scheme
;; (set-popup-rules!
;;     '(("^\\*[gG]eiser \\(dbg\\|xref\\|messages\\)\\*$" :slot 1 :vslot -1)
;;       ("^\\*Geiser documentation\\*$" :slot 2 :vslot 2 :select t :size 0.35)
;;       ("^\\* [A-Za-z0-9_-]+ REPL \\*" :size 0.3 :quit nil :ttl nil)))

;;** Common Lisp
;; (set-popup-rules!
;;     '(("^\\*sly-mrepl"       :vslot 2 :size 0.3 :quit nil :ttl nil)
;;       ("^\\*sly-compilation" :vslot 3 :ttl nil)
;;       ("^\\*sly-traces"      :vslot 4 :ttl nil)
;;       ("^\\*sly-description" :vslot 5 :size 0.3 :ttl 0)
;;       ;; Do not display debugger or inspector buffers in a popup window. These
;;       ;; buffers are meant to be displayed with sufficient vertical space.
;;       ("^\\*sly-\\(?:db\\|inspector\\)" :ignore t)))

;;** Clojure
;; (set-popup-rules!
;;     '(("^\\*cider-error*" :ignore t)
;;       ("^\\*cider-repl" :quit nil :ttl nil)
;;       ("^\\*cider-repl-history" :vslot 2 :ttl nil)))

;;** Org
;; (set-popup-rules!
;;     '(("^\\*Org Links" :slot -1 :vslot -1 :size 2 :ttl 0)
;;       ("^ ?\\*\\(?:Agenda Com\\|Calendar\\|Org Export Dispatcher\\)"
;;        :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :ttl 0)
;;       ("^\\*Org \\(?:Select\\|Attach\\)" :slot -1 :vslot -2 :ttl 0 :size 0.25)
;;       ("^\\*Org Agenda"     :ignore t)
;;       ("^\\*Org Src"        :size 0.42  :quit nil :select t :autosave t :modeline t :ttl nil)
;;       ("^\\*Org-Babel")
;;       ("^\\*Capture\\*$\\|CAPTURE-.*$" :size 0.42 :quit nil :select t :autosave ignore)))

;;** IPython
;; (set-popup-rules!
;;     '(("\\*ob-ipython.*"
;;        :slot 2 :side right :size 100 :height 0.2
;;        :select nil :quit nil :ttl nil)
;;       ("^ \\*Python"
;;        :slot 0 :side right :size 100
;;        :select nil :quit nil :ttl nil)))

;;** Latex
;; (set-popup-rules!
;;   '((" output\\*$" :size 15)
;;     ("^\\*TeX \\(?:Help\\|errors\\)"
;;      :size 0.3 :select t :ttl nil)))

;;** Ledger
;; (set-popup-rules!
;;     '(("^\\*Ledger Report" :size 0.5 :quit 'other :ttl 0)
;;       ("^\\*Ledger Error"  :quit t :ttl 0)))

;;** Personal
;; (set-popup-rules!
;;   '(("^\\*lsp-ui-imenu" :side left :width 60
;;      :vslot -5 :slot 3
;;      :modeline nil :select t :quit t)
;;     ("^\\*Help" :side left :width 60
;;      :vslot -5 :slot 1
;;      :modeline nil :select t :quit t)
;;     ("^\\*Bufler" :side right :width 80
;;      :vslot -5 :slot -5
;;      :modeline nil :select t :quit t)))

;;* Doom Current State
;; (setq doom-buffer-alist
;;       '(("^\\*quickrun"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.3)
;;          (window-width . 40)
;;          (window-height . 0.3)
;;          (slot)
;;          (vslot)
;;          (window-parameters
;;           (ttl . 0)
;;           (quit . t)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)
;;           (transient . t)
;;           (no-other-window . t)))
;;         ("^\\*anaconda-mode"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*lsp-\\(help\\|install\\)"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.35)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select . t)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*cider-repl-history"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot . 2)
;;          (window-parameters
;;           (ttl)
;;           (quit . t)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*cider-repl"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot)
;;          (window-parameters
;;           (ttl)
;;           (quit)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*cider-error*" nil)
;;         ("^\\*envrc\\*"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot)
;;          (window-parameters
;;           (ttl . 0)
;;           (quit . t)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*xref\\*$" nil)
;;         ("^\\*ivy-occur"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.35)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot)
;;          (window-parameters
;;           (ttl . 0)
;;           (quit)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*Flycheck errors\\*"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.25)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*Flycheck error messages\\*"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*org-roam: "
;;          (+popup-buffer)
;;          (actions)
;;          (side . right)
;;          (size)
;;          (window-width . 0.33)
;;          (window-height . 0.5)
;;          (slot . 2)
;;          (vslot)
;;          (window-parameters
;;           (ttl)
;;           (quit)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("\\*org-roam\\*"
;;          (+popup-buffer)
;;          (actions)
;;          (side . right)
;;          (size)
;;          (window-width . 0.33)
;;          (window-height . 0.5)
;;          (slot . 1)
;;          (vslot)
;;          (window-parameters
;;           (ttl)
;;           (quit)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*Capture\\*$\\|CAPTURE-.*$"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.42)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit)
;;           (select . t)
;;           (modeline)
;;           (autosave . ignore)))
;;         ("^\\*Org-Babel"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*Org Src"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.42)
;;          (window-width . 40)
;;          (window-height . 0.42)
;;          (slot)
;;          (vslot)
;;          (window-parameters
;;           (ttl)
;;           (quit)
;;           (select . t)
;;           (modeline . t)
;;           (autosave . t)
;;           (transient . t)
;;           (no-other-window . t)))
;;         ("^\\*Org Agenda" nil)
;;         ("^\\*Org \\(?:Select\\|Attach\\)"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.25)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot . -1)
;;          (vslot . -2)
;;          (window-parameters
;;           (ttl . 0)
;;           (quit . t)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^ ?\\*\\(?:Agenda Com\\|Calendar\\|Org Export Dispatcher\\)"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size function +popup-shrink-to-fit)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot . -1)
;;          (vslot . -1)
;;          (window-parameters
;;           (ttl . 0)
;;           (quit . t)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*Org Links"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 2)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot . -1)
;;          (vslot . -1)
;;          (window-parameters
;;           (ttl . 0)
;;           (quit . t)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*\\(?:[^/]+/[^ ]+ #[0-9]+\\*$\\|Issues\\|Pull-Requests\\|forge\\)" nil)
;;         ("^\\*?[0-9]+:\\(?:new-\\|[0-9]+$\\)"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.45)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot)
;;          (window-parameters
;;           (ttl . 0)
;;           (quit)
;;           (select . ignore)
;;           (modeline . t)
;;           (autosave)))
;;         ("^\\(?:\\*magit\\|magit:\\| \\*transient\\*\\)" nil)
;;         ("^\\*image-dired"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.8)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot . 20)
;;          (vslot)
;;          (window-parameters
;;           (ttl . 0)
;;           (quit)
;;           (select . t)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*Bufler"
;;          (+popup-buffer)
;;          (actions)
;;          (side . right)
;;          (size)
;;          (window-width . 80)
;;          (window-height . 0.16)
;;          (slot . -5)
;;          (vslot . -5)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select . t)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*Help"
;;          (+popup-buffer)
;;          (actions)
;;          (side . left)
;;          (size)
;;          (window-width . 60)
;;          (window-height . 0.16)
;;          (slot . 1)
;;          (vslot . -5)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select . t)
;;           (modeline)
;;           (autosave)
;;           (transient . t)
;;           (no-other-window . t)))
;;         ("^\\*lsp-ui-imenu"
;;          (+popup-buffer)
;;          (actions)
;;          (side . left)
;;          (size)
;;          (window-width . 60)
;;          (window-height . 0.16)
;;          (slot . 3)
;;          (vslot . -5)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select . t)
;;           (modeline)
;;           (autosave)))
;;         ((closure
;;           (t)
;;           (bufname _)
;;           (if
;;               (boundp '+eval-repl-mode)
;;               (progn
;;                 (buffer-local-value '+eval-repl-mode
;;                                     (get-buffer bufname)))))
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.25)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot)
;;          (window-parameters
;;           (ttl closure
;;                (t)
;;                (buf)
;;                (if
;;                    (plist-get +eval-repl-plist :persist)
;;                    nil
;;                  (let*
;;                      ((process
;;                        (and t
;;                             (get-buffer-process buf))))
;;                    (if process
;;                        (progn
;;                          (set-process-query-on-exit-flag process nil)
;;                          (kill-process process)
;;                          (kill-buffer buf))
;;                      nil))))
;;           (quit)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*F\\(?:d\\|ind\\)\\*$" nil)
;;         ("^\\*\\(?:Proced\\|timer-list\\|Abbrevs\\|Output\\|Occur\\|unsent mail.*?\\|message\\)\\*" nil)
;;         ("^\\*Process List\\*"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.25)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot . 101)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select . t)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*Memory-Profiler-Report "
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size)
;;          (window-width . 0.5)
;;          (window-height . 0.4)
;;          (slot . 2)
;;          (vslot . 100)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*CPU-Profiler-Report "
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size)
;;          (window-width . 0.5)
;;          (window-height . 0.4)
;;          (slot . 1)
;;          (vslot . 100)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*Backtrace"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.4)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot . 99)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*Warnings"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.25)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot . 99)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*info\\*$"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.45)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot . 2)
;;          (vslot . 2)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select . t)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*xwidget"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.35)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot . -11)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*eww\\*"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.35)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot . -11)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select . t)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*\\([Hh]elp\\|Apropos\\)"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.42)
;;          (window-width . 40)
;;          (window-height . 0.42)
;;          (slot . 2)
;;          (vslot . -8)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select . t)
;;           (modeline)
;;           (autosave)
;;           (transient . t)
;;           (no-other-window . t)))
;;         ("^ \\*undo-tree\\*"
;;          (+popup-buffer)
;;          (actions)
;;          (side . left)
;;          (size . 20)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot . 2)
;;          (vslot)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select . t)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*Customize"
;;          (+popup-buffer)
;;          (actions)
;;          (side . right)
;;          (size . 0.5)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot . 2)
;;          (vslot)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit)
;;           (select . t)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*Calc"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.4)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot . -7)
;;          (window-parameters
;;           (ttl . 0)
;;           (quit)
;;           (select . t)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*\\(?:Wo\\)?Man "
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.45)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot . -6)
;;          (window-parameters
;;           (ttl . 0)
;;           (quit . t)
;;           (select . t)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*doom:\\(?:v?term\\|e?shell\\)-popup"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.35)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot . -5)
;;          (window-parameters
;;           (ttl)
;;           (quit)
;;           (select . t)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*doom:"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.35)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot)
;;          (vslot . -4)
;;          (window-parameters
;;           (ttl . t)
;;           (quit)
;;           (select . t)
;;           (modeline . t)
;;           (autosave . t)))
;;         ("^\\*\\(?:doom \\|Pp E\\)"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . +popup-shrink-to-fit)
;;          (window-width . 40)
;;          (window-height . +popup-shrink-to-fit)
;;          (slot)
;;          (vslot . -3)
;;          (window-parameters
;;           (ttl . 0)
;;           (quit . t)
;;           (select . ignore)
;;           (modeline)
;;           (autosave . t)
;;           (transient . t)
;;           (no-other-window . t)))
;;         ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . 0.3)
;;          (window-width . 40)
;;          (window-height . 0.3)
;;          (slot)
;;          (vslot . -2)
;;          (window-parameters
;;           (ttl)
;;           (quit . t)
;;           (select . ignore)
;;           (modeline)
;;           (autosave . t)
;;           (transient . t)
;;           (no-other-window . t)))
;;         ("^\\*Local variables\\*$"
;;          (+popup-buffer)
;;          (actions)
;;          (side . bottom)
;;          (size . +popup-shrink-to-fit)
;;          (window-width . 40)
;;          (window-height . 0.16)
;;          (slot . 1)
;;          (vslot . -1)
;;          (window-parameters
;;           (ttl . 5)
;;           (quit . t)
;;           (select . ignore)
;;           (modeline)
;;           (autosave)))
;;         ("^\\*Completions" nil)))

;;; dc-doom-popup.el ends here

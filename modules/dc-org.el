;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2023 David Conner
;; Copyright © 2021 David Wilson
;; Copyright © 2014-2022 Henrik Lissner.
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
;;* Org

(setq org-default-notes-file (dw/org-path "notes.org"))

(straight-use-package '(org :type built-in))

   ;; (setq org-capture-templates
   ;;   '((?b "* READ %?\n\n%a\n\n%:author (%:year): %:title\n   \
   ;;          In %:journal, %:pages.")))

;;* Org

;; hmmm... well that could work... a little too well
;; (defun gv-qset (pairs)
;;   (macroexpand-1 `(setf ,@pairs)))
;; (macroexpand-1 '(setf foo "f1" bar "b2"))

;;** Org Appearance
(defun dc/org-init-appearance-h ()
  (setq org-indirect-buffer-display 'current-window
        ;; turned off by org-indent-mode when the following is set (default)
        ;; org-adapt-indentation nil
        ;; org-indent-mode-turns-off-org-adapt-indentation t
        org-capture-bookmark nil
        org-cycle-separator-lines 2
        org-edit-src-content-indentation 2
        org-eldoc-breadcrumb-separator " → "
        org-ellipsis " ▾"
        org-enforce-todo-dependencies t
        ;; org-entities-user '(("flat"  "\\flat" nil "" "" "266D" "♭")
        ;;                     ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        ;; org-fontify-whole-heading-line t
        org-hide-block-startup nil
        org-hide-emphasis-markers t
        ;; org-hide-leading-stars t
        org-image-actual-width nil
        org-imenu-depth 6
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))
        org-src-fontify-natively t
        ;; `showeverything' is org's default, but it doesn't respect
        ;; `org-hide-block-startup' (#+startup: hideblocks), archive trees,
        ;; hidden drawers, or VISIBILITY properties. `nil' is equivalent, but
        ;; respects these settings.
        ;; org-startup-folded nil
        org-startup-folded 'content
        org-startup-indented t
        ;; org-tags-column 0 ; -77
        org-use-sub-superscripts '{})

  ;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
  ;;      underlying faces like the `org-todo' face does, so we define our own
  ;;      intermediary faces that extend from org-todo.
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))

  ;;*** Latex display
  (plist-put org-format-latex-options :scale 1.5)

  ;;** Refile
  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1))
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path 'file)

  ;; Automatic indent detection in org files is meaningless
  ;; (add-to-list 'doom-detect-indentation-excluded-modes 'org-mode)

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"             ; A task that needs doing & is ready to do
           "PROJ(p)"             ; A project, which usually contains other tasks
           "LOOP(r)"             ; A recurring task
           "STRT(s)"             ; A task that is in progress
           "WAIT(w)"             ; Something external is holding up this task
           "HOLD(h)"             ; This task is paused/on hold because of me
           "IDEA(i)"             ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"    ; Task successfully completed
           "KILL(k)")   ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"                     ; A task that needs doing
           "[-](S)"                     ; Task is in progress
           "[?](W)"                     ; Task is being held up or paused
           "|"
           "[X](D)")                    ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel))))


  (setq org-confirm-babel-evaluate t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t

        ;; org-confirm-babel-evaluate nil
        org-link-elisp-confirm-function 'y-or-n-p
        org-link-shell-confirm-function 'y-or-n-p)

(defun dc/org-init-babel-h ()
  ;; org-confirm-babel-evaluate: set to a function later
  ;; org-src-preserve-indentation:
  ;; - daviwil set to nil, t in .emacs.network
  ;; - see notes on org-adapt-indentation above
  (setq org-confirm-babel-evaluate t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t

        ;; org-confirm-babel-evaluate nil
        org-link-elisp-confirm-function 'y-or-n-p
        org-link-shell-confirm-function 'y-or-n-p)

  (cl-dolist
      (advised '(org-indent-region org-indent-line))
    (advice-add advised :around


     +org-fix-window-excursions-a (fn &rest args)
     "Suppress changes to the window config anywhere
`org-babel-do-in-edit-buffer' is used."
     ;; :around #'evil-org-open-below
     ;; :around #'evil-org-open-above
     :around #'org-indent-region
     :around #'org-indent-line
     (save-window-excursion (apply fn args))))

  ;; (defadvice! +org-fix-newline-and-indent-in-src-blocks-a (&optional indent _arg _interactive)
  ;;  "Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation."
  ;;  :after #'org-return ...)

  ;; (defadvice! +org-inhibit-mode-hooks-a (fn datum name &optional initialize &rest args)
  ;;   "Prevent potentially expensive mode hooks in `org-babel-do-in-edit-buffer' ops."
  ;;   :around #'org-src--edit-element ...)

  ;; (after! ob
  ;;         (add-to-list 'org-babel-default-lob-header-args '(:sync)))

  ;; NOTE: ob will not auto-update images after updates
  ;; - this wasn't working for me AFAIK. i usually needed to hit C-TAB twice
  ;; (add-hook! 'org-babel-after-execute-hook
  ;;            (defun +org-redisplay-inline-images-in-babel-result-h () ...))
  )



;; (add-hook! 'org-mode-hook
;;              ;; `show-paren-mode' causes flickering with indent overlays made by
;;              ;; `org-indent-mode', so we turn off show-paren-mode altogether
;;              #'doom-disable-show-paren-mode-h
;;              ;; disable `show-trailing-whitespace'; shows a lot of false positives
;;              #'doom-disable-show-trailing-whitespace-h
;;              #'+org-enable-auto-reformat-tables-h
;;              #'+org-enable-auto-update-cookies-h
;;              #'+org-make-last-point-visible-h)

;; (add-hook! 'org-load-hook
;;              #'+org-init-org-directory-h
;;              #'+org-init-appearance-h
;;              #'+org-init-agenda-h
;;              #'+org-init-attachments-h
;;              #'+org-init-babel-h
;;              #'+org-init-babel-lazy-loader-h
;;              #'+org-init-capture-defaults-h
;;              #'+org-init-capture-frame-h
;;              #'+org-init-custom-links-h
;;              #'+org-init-export-h
;;              #'+org-init-habit-h
;;              #'+org-init-hacks-h
;;              #'+org-init-keybinds-h
;;              #'+org-init-popup-rules-h
;;              #'+org-init-smartparens-h)

(setup (:pkg org)
  (:also-load org-tempo)
  (:hook dw/org-mode-setup)

  ;;** Modules
  ;; For descriptions, M-x customize-variable org-modules
  (setq org-modules
        '(org-id
          ol-info
          ol-man
          ol-doi
          ol-bibtex
          ol-gnus
          ;; ol-git-link
          org-crypt
          ;; org-bookmark?
          ol-bookmark
          ;; ol-notmuch
          org-protocol
          ))

  ;;*** other org-modules
  ;; org-mouse
  ;; org-tempo: Fast completion for structures
  ;; ol-bibtex
  ;; - https://fossies.org/linux/emacs/lisp/org/ol-bibtex.el
  ;; - https://www.andy-roberts.net/res/writing/latex/bibentries.pdf
  ;; org-ctags
  ;; org-collector
  ;; org-checklist
  ;; org-annotate-file: Annotate a file with Org syntax
  ;; org-notify
  ;; org-panel: reminders
  ;; org-screen
  ;; org-registry: registry for org links
  ;; org-secretary: Team management with Org
  ;; orgtbl-sqlinsert: Convert Org tables to SQL insertions
  ;; org-toc: table of contents
  ;; org-track: keep up with org development

  ;;** Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)))

  ;;*** org-src-lang-modes
  (push '("conf-unix" . conf-unix) org-src-lang-modes)

  ;;** Agenda

  ;;*** Options
  (setq-default
   ;; Different colors for different priority levels
   org-agenda-deadline-faces
   '((1.001 . error)
     (1.0 . org-warning)
     (0.5 . org-upcoming-deadline)
     (0.0 . org-upcoming-distant-deadline))
   ;; Don't monopolize the whole frame just for the agenda
   org-agenda-window-setup 'current-window
   org-agenda-skip-unavailable-files t
   ;; Shift the agenda to show the previous 3 days and the next 7 days for
   ;; better context on your week. The past is less important than the future.
   org-agenda-span 10
   org-agenda-start-on-weekday nil
   org-agenda-start-day "-3d"
   ;; Optimize `org-agenda' by inhibiting extra work while opening agenda
   ;; buffers in the background. They'll be "restarted" if the user switches to
   ;; them anyway (see `+org-exclude-agenda-buffers-from-workspace-h')
   org-agenda-inhibit-startup t))



(provide 'dc-org)

;; TODO: DOOM: configure org-crypt
;; (use-package! org-crypt ; built-in
;;   :commands org-encrypt-entries org-encrypt-entry org-decrypt-entries org-decrypt-entry
;;   :hook (org-reveal-start . org-decrypt-entry)
;;   :preface
;;   ;; org-crypt falls back to CRYPTKEY property then `epa-file-encrypt-to', which
;;   ;; is a better default than the empty string `org-crypt-key' defaults to.
;;   (defvar org-crypt-key nil)
;;   (after! org
;;     (add-to-list 'org-tags-exclude-from-inheritance "crypt")
;;     (add-hook! 'org-mode-hook
;;       (add-hook 'before-save-hook 'org-encrypt-entries nil t))))

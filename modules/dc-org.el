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

;; hmmm... well that could work... a little too well
;; (defun gv-qset (pairs)
;;   (macroexpand-1 `(setf ,@pairs)))
;; (macroexpand-1 '(setf foo "f1" bar "b2"))


;;** Org Hooks
;; these run on org-mode-hook

;; (add-hook! 'org-mode-hook
;;              ;; `show-paren-mode' causes flickering with indent overlays made by
;;              ;; `org-indent-mode', so we turn off show-paren-mode altogether
;;              #'doom-disable-show-paren-mode-h
;;              ;; disable `show-trailing-whitespace'; shows a lot of false positives
;;              #'doom-disable-show-trailing-whitespace-h
;;              #'+org-enable-auto-reformat-tables-h
;;              #'+org-enable-auto-update-cookies-h
;;              #'+org-make-last-point-visible-h)

(defun dc/org-mode-setup ()
  ;; Turn on indentation and auto-fill mode for Org files
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  ;; (visual-line-mode 1)
  (setq-local corfu-auto nil))

;;** Org Load Hooks
;; these run when org first loads

(defun dc/org-init-org-directory-h ()
  (unless org-directory
    (setq-default org-directory (or (getenv "ORG_DIRECTORY")
                                    (file-name-as-directory "~/org"))))
  (unless org-id-locations-file
    (setq org-id-locations-file (expand-file-name ".orgids" org-directory)))

  (setq org-calendars-directory
        (file-name-as-directory (file-name-concat org-directory "calendars"))))

(defun dc/org-init-appearance-h ()
  ;; TODO try org-modern. it is unlikely to conflict but other packages that
  ;; customize faces will no longer be hooked in ...

  ;; ... and i didn't know that (append ... ) supports thee whenlets. it's great
  ;; when evil is not half/dis/advised and features like xref just work (sorry
  ;; doom). i'm sure it works for 99% of the emacs users who use avim. just not
  ;; me.

  ;; (setup (:pkg org-modern)
  ;;   (global-org-modern-mode))
  ;; customs:
  ;; -label-border 'auto
  ;; -star '( stars...)
  ;; -hide-stars 'leading
  ;; -timestamp t
  ;; -table t
  ;; -priority t
  ;; -list '((a . list))
  ;; -checkbox '((a . list))
  ;; -horizontal-rule t
  ;; -todo t
  ;; -todo-faces nil; '((a . list))
  ;; -priority-faces nil; '((a . list))
  ;; -tag t
  ;; -block-name t
  ;; -block-fringe 0
  ;; -keyword t
  ;; -footnote '('defs 'refs)
  ;; -internal-target t
  ;; -radio-target choice
  ;; -statitistics t
  ;; -progress list
  ;;
  ;; org-modern-faces:


  (setup (:pkg org-appear)
    (:hook-into org-mode))

  (setq org-indirect-buffer-display 'current-window
        ;; turned off by org-indent-mode when the following is set (default)
        ;; org-adapt-indentation nil
        ;; org-indent-mode-turns-off-org-adapt-indentation t

        org-capture-bookmark nil
        org-cycle-separator-lines 2
        org-edit-src-content-indentation 0 ; no effect when org-src-preserve-indentation
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
  (require 'ox-latex)
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

;; TODO: see if-let, when-let and doom add-hook! (building func-forms)
;; (defmacro dc/hook-in-derived-modes (mode &rest keyd)
;;   (let (except)
;;     (while (keywordp (car keyd))
;;       (pcase (pop keyd)
;;         (:except (push except (pop keyd)))))
;;     (if-let* ((body (ensure-list keyd))
;;               (keysym? (symbolp (car body))))
;;         (if (fboundp (car body))
;;             ;; TODO bind this-mode
;;             `(dolist (k body)
;;                (if-not (memq this-mode except)
;;                        (add-hook mode #',k)))
;;           `(dolist (mode (,mode))
;;              (if-not (memq this-mode except)
;;                        (add-hook mode )))))
;;     ;; (when-let ((fnsym ((and (fboundp (car body)) (car body)))))
;;     ;;   (dolist (mode)))
;;     ;; (let (body keyd)
;;     ;;   (when symbol))
;;     ))

(defun dw/org-derived-disable-line-numbers ()
  ;; linums are already off unless toggled, but it was a bit tough to find out
  ;; how to handle derived modes. can exclude the parent mode by casing the
  ;; name. must run after derived modes defined (in guix they should already be
  ;; loaded)
  (dolist (mode '(org-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

(defun dc/org-init-agenda-h ()
  (setup org-agenda
    (:option
     ;; start with empty org-agenda-files
     org-agenda-files '()
     org-clock-auto-clockout-timer 300)

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
     org-agenda-inhibit-startup t

     org-log-done 'time
     ;; org-log-into-drawer t
     )

    (org-clock-auto-clockout-insinuate))

  )

(defun dc/org-init-agenda-h ()
  (setup (:pkg org-ql))

  (setup (:pkg org-super-agenda)
    (:load-after org-ql)
    (:option org-super-agenda-header-separator ""

             org-super-agenda-groups
             '((:name "Today" :time-grid t :todo "Today")
               (:habit t)
               (:name "Due today" :deadline today)
               (:name "Overdue" :deadline past)
               (:name "Due soon" :deadline future)
               (:name "Important" :priority "A")
               (:priority<= "B" :order 1)))

    (org-super-agenda-mode +1)))

(defun dc/org-init-roam-h ()

  ;; (setup (:pkg org-roam)
  ;;   (:option))

  (setup (:pkg org-roam-ui)
    (:load-after org-roam)
    (:option org-roam-dailies-directory "dailies/"
             dc/org-roam-dailies-dir
             (file-name-as-directory (concat org-roam-directory
                                             org-roam-dailies-directory))
             dc/most-recent-roam-dailies-take-last 5
             dc/most-recent-roam-dailies
             (-> (directory-files dc/org-roam-dailies-dir nil ".org$")
                 (sort #'string<)
                 (last 5)))
    (defun org-roam-ui-open ()
      "Ensure the server is active, then open the roam graph."
      (interactive)
      (unless org-roam-ui-mode (org-roam-ui-mode 1))
      (browse-url-xdg-open (format "http://localhost:%d" org-roam-ui-port)))))

(defun dc/org-init-attachments-h ()
  )

(defun dc/org-init-babel-h ()
  (setup ob
    (require 'ob-dot)

    ;; org-confirm-babel-evaluate: set to a function later
    ;; org-src-preserve-indentation:
    ;; - daviwil set to nil, t in .emacs.network
    ;; - see notes on org-adapt-indentation above

    (:option org-confirm-babel-evaluate t
             org-src-preserve-indentation t
             org-src-tab-acts-natively t

             ;; default, works pretty well, may obviate the defadvice! below
             org-src-window-setup 'reorganize-frame

             ;; org-confirm-babel-evaluate nil
             org-link-elisp-confirm-function 'y-or-n-p
             org-link-shell-confirm-function 'y-or-n-p))


  ;; TODO org-babel's default async (no session) behavior may cause problems with
  ;; org-exports (if latex/html exports with evaluation doesn't work, this may be the cause)
  ;; (after! ob
  ;;         (add-to-list 'org-babel-default-lob-header-args '(:sync)))
  (setup (:pkg ob-smiles :straight t))

  )

;; NOTE: the advice-add here needs to properly bind the closure
;; - follow defadvice! down to subr.el
;; (cl-dolist
;;     (advised '(org-indent-region org-indent-line))
;;   (advice-add advised :around
;;               +org-fix-window-excursions-a (fn &rest args)
;;               "Suppress changes to the window config anywhere
;; `org-babel-do-in-edit-buffer' is used."
;;               ;; :around #'evil-org-open-below
;;               ;; :around #'evil-org-open-above
;;               :around #'org-indent-region
;;               :around #'org-indent-line
;;               (save-window-excursion (apply fn args))))

;; (defadvice! +org-fix-newline-and-indent-in-src-blocks-a (&optional indent _arg _interactive)
;;  "Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation."
;;  :after #'org-return ...)

;; (defadvice! +org-inhibit-mode-hooks-a (fn datum name &optional initialize &rest args)
;;   "Prevent potentially expensive mode hooks in `org-babel-do-in-edit-buffer' ops."
;;   :around #'org-src--edit-element ...)


;; NOTE: ob will not auto-update images after updates
;; - this wasn't working for me AFAIK. i usually needed to hit C-TAB twice
;; (add-hook! 'org-babel-after-execute-hook
;;            (defun +org-redisplay-inline-images-in-babel-result-h () ...))




;; NOTE: use (with-eval-after-load ...) instead of (after! ...)
;; (add-hook! 'org-load-hook


(defun dc/org-init-babel-lazy-loader-h ()
  )
(defun dc/org-init-capture-defaults-h ()
  (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

  ;; TODO doom/personal capture templates
  (add-hook 'org-after-refile-insert-hook #'save-buffer))
(defun dc/org-init-capture-frame-h ()

  )
(defun dc/org-init-custom-links-h ()

  )

(defun dc/org-init-formatting-h ()
  (setup (:pkg org-make-toc)
    (:option org-toc-default-depth 1)
    (:hook-into org-mode)))
(defun dc/org-init-export-h ()
  (setq org-export-headline-levels 5)

  ;; TODO ox-extra: enable :ignore: headlines (in addition to :noexport:)
  ;; (require 'ox-extra)
  ;; (ox-extras-activate '(ignore-headlines))
  )
(defun dc/org-init-habit-h ()

  )
(defun dc/org-init-hacks-h ()

  )
(defun dc/org-init-keybinds-h ()

  )
(defun dc/org-init-popup-rules-h ()

  )

;; (defun dc/org-init-smartparens-h ())

(require 'org-eldoc)

(setup (:pkg org-tempo)
  (:load-after org)
  (:when-loaded
    (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
    (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
    (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("go" . "src go"))
    (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
    (add-to-list 'org-structure-template-alist '("json" . "src json"))))

;;** Org Setup
(setup (:pkg org)
  (:also-load org-tempo
              org-eldoc)
  (:hook dc/org-mode-setup)

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

  (dc/org-init-org-directory-h)
  (dc/org-init-appearance-h)
  (dc/org-init-agenda-h)
  (dc/org-init-roam-h)
  (dc/org-init-attachments-h)
  (dc/org-init-babel-h)
  (dc/org-init-babel-lazy-loader-h)
  (dc/org-init-capture-defaults-h)
  (dc/org-init-capture-frame-h)
  (dc/org-init-custom-links-h)
  (dc/org-init-formatting-h)
  (dc/org-init-export-h)
  (dc/org-init-habit-h)
  (dc/org-init-hacks-h)
  (dc/org-init-keybinds-h)
  (dc/org-init-popup-rules-h)
  ;; (dc/org-init-smartparens-h)

  ;; TODO: doom ignores org-babel-do-load-languages and lazy loads
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)))

  ;;*** org-src-lang-modes
  (push '("conf-unix" . conf-unix) org-src-lang-modes)

  ;;** Agenda

  ;;*** Options


  (setq org-tag-persistent-alist
        '((:startgroup . nil)
          ("VIS" . ?v)
          ("ISH" . ?!)
          ("GO" . ?G)
          ("FIN" . ?$) (:newline . nil)
          (:endgroup . nil) (:startgroup . nil)
          ("AUTO" . ?a)
          ("NET" . ?n)
          ("FS" . ?f)
          ("DO" . ?d)
          ("AU" . ?@)
          ("ID" . ?#)
          ("DF" . ?.) (:newline . nil)
          (:endgroup . nil) (:startgroup . nil)
          ("CODEX" . ?%)
          ("3D" . ?3)
          ("CAD" . ?C)
          ("WS" . ?w)
          ("ART" . ?A)
          ("MUS" . ?M)
          ("LEARN" . ?L)
          ("EDU" . ?E)
          ("HOME" . ?H)
          ("FAB" . ?F) (:newline . nil)
          (:endgroup . nil) (:startgroup . nil)
          ("MEET" . ?M)
          ("MSG" . ?m)
          ("EV" . ?V)
          ("CON" . ?c) (:newline . nil)
          (:endgroup . nil))))


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

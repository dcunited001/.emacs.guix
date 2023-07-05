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
  (setq-local corfu-auto nil
              help-at-pt-display-when-idle t
              help-at-pt-timer-delay 0.5))

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
        org-hide-emphasis-markers nil
        org-hide-macro-markers nil
        ;; org-hide-leading-stars t
        org-image-actual-width nil
        org-imenu-depth 6
        org-priority-default ?A
        org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-faces '((?A . error)
                             (?B . error)
                             (?C . warning)
                             (?D . warning)
                             (?E . success))
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
  (setq org-refile-targets `((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 2)
                             (org-default-notes-file :maxlevel . 2))
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path 'file)

  ;; Automatic indent detection in org files is meaningless
  ;; (add-to-list 'doom-detect-indentation-excluded-modes 'org-mode)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)"
                    "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)"
                    "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))
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

;; TODO: script more behavior with (org-notify notification &optional pay-sound)
;; - calls (org-clock-play-sound play-sound)
;; - requires aplay. extremely loud noise when aplay plays .oga files
;; There are two actions in the org source which result in org-notify calls:
;; - via setting effort: org-clock-notify-once-if-expired
;; - via setting timers: org-timer-pause-or-continue/org-timer-set-timer
(defun dc/org-clock-play-sound (&optional clock-sound)
  (let ((org-clock-sound (or clock-sound org-clock-sound)))
    (cond
     ((not org-clock-sound))
     ((eq org-clock-sound t) (beep t) (beep t))
     ((stringp org-clock-sound)
      (let ((file (expand-file-name org-clock-sound)))
	      (if (file-exists-p file)
	          (if (executable-find "playsound")
		            (start-process "org-clock-play-notification" nil
			                         "playsound" file)
	            (condition-case nil
		              (play-sound-file file)
		            (error (beep t) (beep t))))))))))

;; hmmmm is it possible to (cl-flet (( #'function-by-symbol)) ... )?
(if (executable-find "playsound")
    (advice-add 'org-clock-play-sound :override #'dc/org-clock-play-sound)
  (warn "org-clock-sound: Can't find `playsound`. Not overriding org-clock-play-sound")
  (setq org-clock-sound nil))

(defun dc/org-init-agenda-h ()
  (setup org-agenda
    (:option
     ;; start with empty org-agenda-files
     org-agenda-files '()
     org-clock-auto-clockout-timer 300
     org-clock-history-length 25

     org-clock-in-switch-to-state "STRT"
     org-clock-out-switch-to-state "HOLD"
     org-clock-out-remove-zero-time-clocks t

     org-log-done 'time
     ;; org-log-into-drawer t

     )

    (and (file-exists-p dc/emacs-sound-theme-path)
         (setq org-clock-sound (expand-file-name "complete.oga" dc/emacs-sound-theme-path)))

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
     )

    (org-clock-auto-clockout-insinuate))

  (setup (:pkg org-ql))

  ;; needs 1.3-pre for :take selector, guix @ 1.2
  (setup (:pkg org-super-agenda :straight t :type git :flavor melpa
               :host github :repo "alphapapa/org-super-agenda")

    (:option org-super-agenda-header-separator ""

             org-super-agenda-groups
             '((:name "Today" :time-grid t :todo "TODO")
               ;; (:habit t)
               (:name "Due today" :deadline today)
               (:name "Overdue" :deadline past)
               (:name "Due soon" :deadline future)
               (:name "Urgent" :priority "A")
               (:name "Crit" :priority "B")
               (:name "No Estimate" :scheduled t)
               (:name "No Deadline" :scheduled t)
               (:priority<= "C" :order 1)))

    (org-super-agenda-mode +1)))

;; (defun dc/org-super-agenda-queries ()
;;     )

;; org-roam-node-display-template:
;;
;; the default gives titles that are too narrow (12)
;; org-roam-node--* sends the width of the then-current buffer
;; and the completing-read functionality is adjusted for ~80 chars
;;
;; requires defining org-roam-node-doom-hierarchy
;; (format "${doom-hierarchy:36} %s %s"
;;         (propertize "${doom-type:*}" 'face 'font-lock-keyword-face)
;;         (propertize "${doom-tags:18}" 'face 'org-tag))

;; org-roam-extract-new-file-path
;;
;; doesn't work with a "slips/" path prepended to it

;; (doom-load-packages-incrementally
;;  '(ansi-color dash f rx seq magit-section emacsql emacsql-sqlite))

;; org-roam-node-display-template

(defun dc/org-read-template-from-file (file)
  (if (file-exists-p file) (org-file-contents file)
    (error "* Template file %s not found" file)))


(defun dc/org-roam-insert-slug ()
  (interactive)
  (insert (org-roam-node-slug (org-roam-node-at-point))))

(defun dc/org-roam-get-slug ()
  (interactive)
  (org-roam-node-slug (org-roam-node-at-point)))

(defun dc/org-init-roam-h ()
  (require 'doom-org-roam2)

  (setup (:pkg org-roam)
    (:option
     org-roam-extract-new-file-path "${slug}-%<%Y%m%d%H%M%S>-.org"
     ;; org-roam-node-display-template

     org-roam-list-files-commands '(fd fdfind rg find)
     org-roam-db-gc-threshold most-positive-fixnum
     org-roam-mode-section-functions #'(org-roam-backlinks-section
                                        org-roam-reflinks-section)
     org-roam-completion-everywhere nil)

    ;; (add-to-list 'org-roam-node-template-prefixes '("doom-tags" . "#"))
    ;; (add-to-list 'org-roam-node-template-prefixes '("doom-type" . "@"))
    ;; (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode

    ;; this should work, but seems unimplemented in org-roam
    ;; (file "./relative/path/from/roam/template.org")
    (setq org-roam-dailies-capture-templates
          `(("d" "default" entry "%?" :target
             (file+head "%<%Y-%m-%d>.org"
                        ,(dc/org-read-template-from-file
                          dc/org-roam-dailies-template)))))
    (setq org-roam-capture-templates
          (append
           '(("d" "default"
              plain "%?" :unnarrowed t
              :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n"))
             ("p" "projects"
              plain "%?" :unnarrowed t
              :target (file+head
                       "projects/${slug}.org"
                       "#+TITLE: ${title}\n#+DESCRIPTION: ${description}\n"))
             ("t" "topics"
              plain "%?" :unnarrowed t
              :target (file+head+olp
                       "topics/${slug}.org"
                       "#+TITLE: ${title}\n#+DESCRIPTION: ${description}\n#+TAGS:\n\n"
                       ("Roam" "Docs" "Resources" "Topics" "Issues")))
             ("c" "code"
              plain "%?" :unnarrowed t
              :target (file+head+olp
                       "code/${slug}.org"
                       "#+TITLE: ${title}\n#+DESCRIPTION: ${description}\n#+TAGS:\n\n"
                       ("Roam" "Docs" "Resources" "Issues" "Projects"))))
           `(("s" "slips"
              plain "%?" :unnarrowed t
              :target (file+head+olp
                       "slips/%<%Y%m%d%H%M%S>-${slug}.org"
                       ,(string-join '("#+TITLE: ${title}"
                                       "#+CATEGORY: slips"
                                       "#+TAGS: ") "\n")
                       ("Roam" "Docs" "Resources" "Issues" "Projects"))))))



    ;; (setq-hook! 'org-roam-find-file-hook
    ;;             org-id-link-to-org-use-id
    ;;             +org-roam-link-to-org-use-id)
    ;; (:hook turn-on-visual-line-mode)
    (:with-hook desktop-after-read-hook
      (:hook org-roam-db-autosync-enable)))

  (setup (:pkg org-roam-ui)
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
      (browse-url-xdg-open (format "http://localhost:%d" org-roam-ui-port))))

  ;; (advice-add #'org-roam-link-follow-link :filter-args #'org-roam-link-follow-link-with-description-a)
  (advice-add #'org-roam-link-replace-at-point :override #'org-roam-link-replace-at-point-a))

(with-eval-after-load 'org-roam
  (setup (:pkg consult-org-roam :straight t :type git :flavor melpa
               :host github :repo "jgru/consult-org-roam")
    (:option consult-org-roam-grep-func #'consult-ripgrep
             consult-org-roam-buffer-narrow-key consult-narrow-key
             consult-org-roam-buffer-after-buffers t)

    (require 'consult-org-roam)
    (consult-customize
     consult-org-roam-forward-links
     :preview-key (kbd "M-."))
    (consult-org-roam-mode 1)))

(defun dc/org-init-attachments-h ()
  )

;; set with (setq org-confirm-evaluate #'dc/org-babel-dont-confirm-shell-elisp)
(defun dc/org-babel-dont-confirm-shell-elisp (lang body)
  (not (or (string= lang "emacs-lisp")
           (string= lang "shell"))))

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

;; the logic here is copied from doom-emacs
(defvar dc/org-capture-todo-file "todo.org")
(defvar dc/org-capture-notes-file "notes.org")
(defvar dc/org-capture-journal-file "journal.org")
(defvar dc/org-capture-changelog-file "changelog.org")
(defvar dc/org-capture-projects-file "projects.org")

(defun dc/org-capture-local-root (path)
  (let ((filename (file-name-nondirectory path)))
    (expand-file-name
     filename
     (or ;; (locate-dominating-file (file-truename default-directory)
      ;;                         filename)
      (and (project-current) (cdr (project-current)))
      (user-error "Couldn't detect a project")))))

(defun dc/org-capture-project-todo-file ()
  (dc/org-capture-local-root dc/org-capture-todo-file))
(defun dc/org-capture-project-notes-file ()
  (dc/org-capture-local-root dc/org-capture-notes-file))
(defun dc/org-capture-project-changelog-file ()
  (dc/org-capture-local-root dc/org-capture-changelog-file))

(defun dc/org-init-capture-defaults-h ()
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline dc/org-capture-todo-file "Inbox")
           "* [ ] %?\n%i\n%a" :prepend t)

          ("n" "Personal notes" entry
           (file+headline dc/org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `dc/org-capture-todo-file',
          ;; `dc/org-capture-changelog-file' and `dc/org-capture-notes-file'.
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry ; {project-root}/todo.org
           (file+headline dc/org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry ; {project-root}/notes.org
           (file+headline dc/org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry ; {project-root}/changelog.org
           (file+headline dc/org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)

          ;; Will use {org-directory}/{dc/org-capture-projects-file} and store
          ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
          ;; support `:parents' to specify what headings to put them under, e.g.
          ;; :parents ("Projects")
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           (function dc/org-capture-central-project-todo-file)
           "* TODO %?\n %i\n %a"
           :heading "Tasks"
           :prepend nil)
          ("on" "Project notes" entry
           (function dc/org-capture-central-project-notes-file)
           "* %U %?\n %i\n %a"
           :heading "Notes"
           :prepend t)
          ("oc" "Project changelog" entry
           (function dc/org-capture-central-project-changelog-file)
           "* %U %?\n %i\n %a"
           :heading "Changelog"
           :prepend t)))
  ;; TODO doom/personal capture templates
  (add-hook 'org-after-refile-insert-hook #'save-buffer))

(defun dc/org-init-capture-frame-h ()

  )

;; DOOM: ./lisp/core/doom-lib.el
(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defun dc/org-init-custom-links-h ()

  ;; DOOM: ./modules/lang/org/config.el
  ;; Modify default file: links to colorize broken file links
  (org-link-set-parameters
   "file" :face (lambda (path)
                  (if (or (file-remote-p path)
                          ;; filter out network shares on windows (slow)
                          (if IS-WINDOWS (string-prefix-p "\\\\" path))
                          (file-exists-p path))
                      'org-link
                    '(warning org-link))))

  ;; TODO: DOOM org: see +org-define-basic-link

  ;; TODO: potentially load with org-link-abbrev-alist.eld

  ;; DOOM: ./modules/lang/org/config.el
  (pushnew! org-link-abbrev-alist
            '("github"      . "https://github.com/%s")
            '("nyxt"        . "https://nyxt.atlas.engineer/documentation#%s")
            '("youtube"     . "https://youtube.com/watch?v=%s")
            '("google"      . "https://google.com/search?q=")
            '("gimages"     . "https://google.com/images?q=%s")
            '("gmap"        . "https://maps.google.com/maps?q=%s")
            '("duckduckgo"  . "https://duckduckgo.com/?q=%s")
            '("wikipedia"   . "https://en.wikipedia.org/wiki/%s")
            '("wolfram"     . "https://wolframalpha.com/input/?i=%s")
            '("doom-repo"   . "https://github.com/doomemacs/doomemacs/%s"))

  ;; TODO: DOOM org: giant letf for doc links, http/img previews,
  ;; TODO: DOOM org: +org--follow-search-string

  (pushnew! org-link-abbrev-alist
            `("emacsdir"    . (file-name-concat dc/emacs-d "%s"))))

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

(defun dc/catch-org-shiftselect-error (newfun oldfun &rest args)
  (condition-case err
      (funcall oldfun args)
    ;; on error, run handler (funcall ...)
    (error (funcall newfun))))

(defun dc/org-fix-buf-move ()
  (advice-add 'org-shiftcontrolright
              :around (apply-partially
                       #'dc/catch-org-shiftselect-error
                       #'buf-move-right)
              '(name "dc/catch-org-shiftcontrolright"))
  (advice-add 'org-shiftcontrolleft
              :around (apply-partially
                       #'dc/catch-org-shiftselect-error
                       #'buf-move-left)
              '(name "dc/catch-org-shiftcontrolright"))
  (advice-add 'org-shiftcontrolup
              :around (apply-partially
                       #'dc/catch-org-shiftselect-error
                       #'buf-move-up)
              '(name "dc/catch-org-shiftcontrolright"))
  (advice-add 'org-shiftcontroldown
              :around (apply-partially
                       #'dc/catch-org-shiftselect-error
                       #'buf-move-down)
              '(name "dc/catch-org-shiftcontrolright")))

(defun dc/org-init-keybinds-h ()
  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-M-RET-may-split-line '((default . nil)))
  (dc/org-fix-buf-move))

(defun dc/org-init-popup-rules-h ()

  )

;; (defun dc/org-init-smartparens-h ())

(with-eval-after-load 'org
  (setup (:pkg org-contrib))

  ;; TODO org-eldoc doesn't seem to document thing
  (require 'org-eldoc)

  ;; TODO configure org-tempo
  (setup (:pkg org-tempo)
    (:when-loaded
      (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
      (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
      (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
      (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
      (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
      (add-to-list 'org-structure-template-alist '("py" . "src python"))
      (add-to-list 'org-structure-template-alist '("go" . "src go"))
      (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
      (add-to-list 'org-structure-template-alist '("json" . "src json")))))

;;** Org Setup
(setup (:pkg org)
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
          org-notify))

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
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (jq . t)))

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

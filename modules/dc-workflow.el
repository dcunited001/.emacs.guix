;; -*- lexical-binding: t; -*
;;
;; Copyright © 2023 David Conner
;; Copyright © 2021 David Wilson
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

;;;* ORG
(require 'dw-org)

;; '("Cadl.org" "Personal.org" "Mesche.org" "SystemCrafters.org")
(setq org-agenda-files nil)

(defun dw/org-path (path)
  (expand-file-name path org-directory))

(setq org-default-notes-file (dw/org-path "Inbox.org"))

;; Turn on indentation and auto-fill mode for Org files
(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq corfu-auto nil)
  (setq evil-auto-indent nil))

;; Make sure Straight pulls Org from Guix
;;(when dw/is-guix-system
  (straight-use-package '(org :type built-in))
  ;)

;;** ORG

;;*** org-mode main config

(setup (:pkg org)
       (:also-load org-tempo)
       (:hook dw/org-mode-setup)
       (setq org-ellipsis " ▾"
             org-hide-emphasis-markers t
             org-src-fontify-natively t
             org-fontify-quote-and-verse-blocks t
             org-src-tab-acts-natively t
             org-edit-src-content-indentation 2
             org-hide-block-startup nil
             org-src-preserve-indentation nil
             org-startup-folded 'content
             org-cycle-separator-lines 2
             org-capture-bookmark nil)

       (setq org-modules
             '(org-crypt
               ;; org-habit
               ;; org-eshell
               ;; org-irc
               org-bookmark))

       (setq org-refile-targets '((nil :maxlevel . 1)
                                  (org-agenda-files :maxlevel . 1)))

       (setq org-outline-path-complete-in-steps nil)
       (setq org-refile-use-outline-path t)

       (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
       (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

       (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
       (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)

       (org-babel-do-load-languages
        'org-babel-load-languages
        '((emacs-lisp . t)))
                                        ;(ledger . t))) -- Not working right now

       (push '("conf-unix" . conf-unix) org-src-lang-modes))

;;*** org-faces

;; TODO: Pull from Guix
(setup (:pkg org-modern :straight t)
       (global-org-modern-mode))

;; (unless dw/is-termux
;;   (setup (:pkg org-superstar)
;;     (:load-after org)
;;     (:hook-into org-mode)
;;     (:option org-superstar-remove-leading-stars t
;;              org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))))

;; Replace list hyphen with dot
;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setup org-faces
       ;; Make sure org-indent face is available
       (:also-load org-indent)
       (:when-loaded
        ;; Increase the size of various headings
        (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)

        (dolist (face '((org-level-1 . 1.2)
                        (org-level-2 . 1.1)
                        (org-level-3 . 1.05)
                        (org-level-4 . 1.0)
                        (org-level-5 . 1.1)
                        (org-level-6 . 1.1)
                        (org-level-7 . 1.1)
                        (org-level-8 . 1.1)))
          (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

        ;; Ensure that anything that should be fixed-pitch in Org files appears that way
        (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
        (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
        (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
        (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
        (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
        (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
        (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
        (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
        (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

        ;; Get rid of the background on column views
        (set-face-attribute 'org-column nil :background nil)
        (set-face-attribute 'org-column-title nil :background nil)))

;; TODO: Others to consider
;; '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;; '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-property-value ((t (:inherit fixed-pitch))) t)
;; '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;; '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;; '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;;*** org-tempo
;; autoexpand snippets
;; This is needed as of Org 9.2
(setup org-tempo
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

;;*** org-agenda packages

;;*** org-agenda config

;;**** org-clock

;;*** org-roam

;;**** org-roam-protocol

;;*** org-roam: daviwil

;;****  Project Templates

;;**** Roam Node Insert

;;**** Roam Capture Task: project captures

;;*** org-capture

;;**** org-capture protocols

(require 'org-protocol)

;;*** org-refile

;;*** org-mode misc

;;**** org-krita

;;**** org-drill

;;**** org-treeusage

;;**** org-make-toc

(setup (:pkg org-make-toc)
  (:hook-into org-mode))

;;**** org-appear

(setup (:pkg org-appear)
  (:hook-into org-mode))

;;*** org-mode keys




;; TODO org-roam


(defvar dw/org-roam-project-template
  '("p" "project" plain "** TODO %?"
    :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: ${title}\n#+category: ${title}\n#+filetags: Project\n"
                           ("Tasks"))))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun dw/org-roam-goto-month ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("m" "month" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "%<%Y-%B>.org"
                                                      "#+title: %<%Y-%B>\n#+filetags: Project\n")
                                   :unnarrowed t))))

(defun dw/org-roam-goto-year ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("y" "year" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "%<%Y>.org"
                                                      "#+title: %<%Y>\n#+filetags: Project\n")
                                   :unnarrowed t))))

(defun dw/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "Project"))
                     :templates (list dw/org-roam-project-template)))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

(defhydra dw/org-roam-jump-menu (:hint nil)
  "
^Dailies^        ^Capture^       ^Jump^
^^^^^^^^-------------------------------------------------
_t_: today       _T_: today       _m_: current month
_r_: tomorrow    _R_: tomorrow    _e_: current year
_y_: yesterday   _Y_: yesterday   ^ ^
_d_: date        ^ ^              ^ ^
"
  ("t" org-roam-dailies-goto-today)
  ("r" org-roam-dailies-goto-tomorrow)
  ("y" org-roam-dailies-goto-yesterday)
  ("d" org-roam-dailies-goto-date)
  ("T" org-roam-dailies-capture-today)
  ("R" org-roam-dailies-capture-tomorrow)
  ("Y" org-roam-dailies-capture-yesterday)
  ("m" dw/org-roam-goto-month)
  ("e" dw/org-roam-goto-year)
  ("c" nil "cancel"))

(setup (:pkg org-roam)
  (setq org-roam-v2-ack t)
  (setq dw/daily-note-filename "%<%Y-%m-%d>.org"
        dw/daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")

 (:when-loaded
    (org-roam-db-autosync-mode)
    ;; (my/org-roam-refresh-agenda-list)
    )

  (:option
   org-roam-directory "~/Notes/Roam/"
   org-roam-dailies-directory "Journal/"
   org-roam-completion-everywhere t
   org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t))
   org-roam-dailies-capture-templates
   `(("d" "default" entry
      "* %?"
      :if-new (file+head ,dw/daily-note-filename
                         ,dw/daily-note-header))
     ("t" "task" entry
      "* TODO %?\n  %U\n  %a\n  %i"
      :if-new (file+head+olp ,dw/daily-note-filename
                             ,dw/daily-note-header
                             ("Tasks"))
      :empty-lines 1)
     ("l" "log entry" entry
      "* %<%I:%M %p> - %?"
      :if-new (file+head+olp ,dw/daily-note-filename
                             ,dw/daily-note-header
                             ("Log")))
     ("j" "journal" entry
      "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
      :if-new (file+head+olp ,dw/daily-note-filename
                             ,dw/daily-note-header
                             ("Log")))
     ("m" "meeting" entry
      "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
      :if-new (file+head+olp ,dw/daily-note-filename
                             ,dw/daily-note-header
                             ("Log")))))
  (:global "C-c n l" org-roam-buffer-toggle
           "C-c n f" org-roam-node-find
           "C-c n d" dw/org-roam-jump-menu/body
           "C-c n c" org-roam-dailies-capture-today
           "C-c n t" dw/org-roam-capture-task
           "C-c n g" org-roam-graph)
  (:with-map org-mode-map
    (:bind "C-c n i" org-roam-node-insert
           "C-c n I" org-roam-insert-immediate)))


(with-eval-after-load 'org-roam
  (defun my/org-roam-project-finalize-hook ()
    "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
    ;; Remove the hook since it was added temporarily
    (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Add project file to the agenda list if the capture was confirmed
    (unless org-note-abort
      (with-current-buffer (org-capture-get :buffer)
        (add-to-list 'org-agenda-files (buffer-file-name)))))

  (defun my/org-roam-find-project ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
     nil
     nil
     (my/org-roam-filter-by-tag "Project")
     :templates
     '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
        :unnarrowed t))))

  (defun my/org-roam-capture-inbox ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create)
                       :templates '(("i" "inbox" plain "* %?"
                                     :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

  (defun my/org-roam-copy-todo-to-today ()
    (interactive)
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
          (org-roam-dailies-capture-templates
           '(("t" "tasks" entry "%?"
              :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos)
      (save-window-excursion
        (org-roam-dailies--capture (current-time) t)
        (setq today-file (buffer-file-name))
        (setq pos (point)))

      ;; Only refile if the target file is different than the current file
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos)))))

  ;; (add-to-list 'org-after-todo-state-change-hook
  ;;              (lambda ()
  ;;                (when (equal org-state "DONE")
  ;;                  (my/org-roam-copy-todo-to-today))))
  )

(setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
    (sequence "|" "WAIT(w)" "BACK(b)")))

;; TODO: org-todo-keyword-faces
(setq org-todo-keyword-faces
  '(("NEXT" . (:foreground "orange red" :weight bold))
    ("WAIT" . (:foreground "HotPink2" :weight bold))
    ("BACK" . (:foreground "MediumPurple3" :weight bold))))

;; Configure common tags
(setq org-tag-alist
  '((:startgroup)
     ; Put mutually exclusive tags here
     (:endgroup)
     ("@home" . ?H)
     ("@work" . ?W)
     ("batch" . ?b)
     ("followup" . ?f)))

(setq org-agenda-window-setup 'current-window)
(setq org-agenda-span 'day)
(setq org-agenda-start-with-log-mode t)

;; Make done tasks show up in the agenda log
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-columns-default-format "%20CATEGORY(Category) %65ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock) %TAGS")

(setq org-agenda-custom-commands
      `(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "High Priority")))
          (tags-todo "+followup" ((org-agenda-overriding-header "Needs Follow Up")))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Actions")
                 (org-agenda-max-todos nil)))
          ))

        ("n" "Next Tasks"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ;; Low-effort next actions
        ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Tasks")
          (org-agenda-max-todos 20)
          (org-agenda-files org-agenda-files)))))

(add-hook 'org-timer-set-hook #'org-clock-in)

(defun dw/get-todays-journal-file-name ()
  "Gets the journal file name for today's date"
  (interactive)
  (let* ((journal-file-name
           (expand-file-name
             (format-time-string "%Y/%Y-%2m-%B.org")
             (dw/org-path "Journal/")))
         (journal-year-dir (file-name-directory journal-file-name)))
    (if (not (file-directory-p journal-year-dir))
      (make-directory journal-year-dir))
    journal-file-name))


(defun dw/on-org-capture ()
  ;; Don't show the confirmation header text
  (setq header-line-format nil)

  ;; Control how some buffers are handled
  (let ((template (org-capture-get :key t)))
    (pcase template
      ("jj" (delete-other-windows)))))

(add-hook 'org-capture-mode-hook 'dw/on-org-capture)

(setq org-capture-templates
  `(("t" "Tasks")
    ("tt" "Task" entry (file ,(dw/org-path "Inbox.org"))
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
    ("ts" "Clocked Entry Subtask" entry (clock)
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

    ("j" "Journal Entries")
    ("je" "General Entry" entry
         (file+olp+datetree ,(dw/org-path "Journal.org"))
         "\n* %<%I:%M %p> - %^{Title} \n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)
    ("jt" "Task Entry" entry
         (file+olp+datetree ,(dw/org-path "Journal.org"))
         "\n* %<%I:%M %p> - Task Notes: %a\n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)
    ("jj" "Journal" entry
         (file+olp+datetree ,(dw/org-path "Journal.org"))
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)))

(provide 'dc-workflow)

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

;;* Interface

;;** Basics

(setq inhibit-startup-message t)

;;*** Tooltips

;; there are realgud/dap integrations for inspecting vars/etc with tooltip
(setq-default tooltip-delay 0.7                 ;2.0
              tooltip-short-delay 0.1)          ;0.5

;; TODO: determine whether there are possible issues with pgtk/wayland?
;; (tooltip-mode -1)       ; Disable tooltips
(tooltip-mode +1)

;;*** Menus

;; menu-bar-mode gets a bad rap from tool-bar-mode
(menu-bar-mode +1)            ; Enable the menu bar
(context-menu-mode +1)
(tool-bar-mode -1)         ; Disable the toolbar

;;*** Date & Time

(setq display-time-world-list
      '(("Etc/UTC" "UTC")
        ("Europe/Athens" "Athens")
        ("America/Los_Angeles" "Seattle")
        ("America/Denver" "Denver")
        ("America/New_York" "New York")
        ("Pacific/Auckland" "Auckland")
        ("Asia/Shanghai" "Shanghai")
        ("Asia/Kolkata" "Hyderabad")))

(setq-default display-time-world-time-format "%a, %d %b %I:%M %p %Z"
              display-time-format "%l:%M %p %b %d W%U"
              display-time-load-average-threshold 0.0)

;;*** Mouse

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      ;; mouse-wheel-scroll-amount '(8)
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse t        ;; scroll window under mouse
      mouse-drag-and-drop-region t
      scroll-step 1 ;; keyboard scroll one line at a time
      use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

(setq-default visible-bell t)

;; (set-frame-parameter (selected-frame) 'alpha-background 90)
;; (add-to-list 'default-frame-alist '(alpha-background 90))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Use UTF-8 by default
(set-default-coding-systems 'utf-8)

;;(setq-default large-file-warning-threshold nil)
(setq-default vc-follow-symlinks t
              ;; "Disable all version control. makes startup and opening files much
              ;; faster except git and svn which I actually use" - jkitchin
              vc-handled-backends '(Git SVN))
(setq-default ad-redefinition-action 'accept)

;;** Search

;;*** Xref

;; xref needs to be loaded before pulsar, xref-find-* can't xref itself (and
;; help-mode points to the wrong locations). (setup xref ...) may be what's
;; causing this to happen


(setq xref-file-name-display 'project-relative
      ;; conflicts with consult-xref config
      ;; xref-show-definitions-function #'xref-show-definitions-completing-read
      ;; xref-show-xrefs-function #'xref-show-definitions-buffer

      xref-search-program
      (cond
       ((executable-find "ugrep") 'ugrep)
       ((or (executable-find "ripgrep") (executable-find "rg")) 'ripgrep)
       (t 'grep)))

;; TODO find a way to close the stack buffers, but this notes the files i visited
(defun dc/pp-xref-stack (&optional arg)
  (interactive "p")

  ;; FIXME: (statement returns correctly when outside of let blockdoesn't return anything ... no time)
  (let ((xref--history
         (if (> 1 arg)
             xref--history
           (list (car xref--history)))))
    (mapcar (lambda (xr)
              (string-join
               (->> xr
                    ;; (identity)
                    (mapcar #'marker-buffer)
                    (mapcar #'buffer-name)) ","))
            xref--history)))

;;*** Grep

(defvar dc/grep-ignored-directories '("po")
  "directories to pass to grep tools")

;; ripgrep:
;; - prefer using .rgignore, .gitignore or --ignore-file
;; - some regexp pref comparison stats are cherry-picked

(defvar dc/ripgrep-args "-g \"!/po\""
  "args to pass to ripgrep")

;; NOTE: the grep-files-aliases seems to work with rgrep, but not
;; project-find-regepx

(use-package grep :straight (:type built-in)
  :config
  (cl-dolist (a '(("tt" . "*[-_][Tt]est*")
                  ("ss" . "*[-_][Ss]spec*")
                  ("ht" . "*.html")
                  ("xx" . "*.xml")
                  ("jj" . "*.json")
                  ("yy" . "*.yml *.yaml")
                  ("tt" . "todo.org")
                  ("nn" . "notes.org")
                  ("dr" . ".dir-locals.el")
                  ("mm" . "*[Mm]ain*")
                  ("gi" . ".gitignore")))
    (add-to-list 'grep-files-aliases a))
  (cl-dolist (d dc/grep-ignored-directories)
    (add-to-list 'grep-find-ignored-directories d)))

(defun dc/project-find-regexp (regexp)
  (interactive (list (project--read-regexp)))
  (let ((current-prefix-arg (ash #x1 2)))
    (project-find-regexp regexp)))

;;** Buffers

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t
      ;; prevents auto-revert-mode from displaying constant "reverting buffer"
      ;; messages in the echo area
      auto-revert-verbose nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;;*** Bufler

;; NOTE: bufler-defauto-group: conditions defining "auto" groups
;; https://github.com/alphapapa/bufler.el/blob/master/bufler.el#L1100-L1170

;; (setq dc/bufler-special
;;       (quote (group-and "*Special*"
;;                         (lambda (buffer)
;;                           (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
;;                                                buffer)
;;                                       (funcall (mode-match "Dired" (rx bos "dired"))
;;                                                buffer)
;;                                       (funcall (auto-file) buffer))
;;                             "*Special*"))))
;;       dc/bufler-special-special)

;;**** Bufler defauto-groups

(defun dc/bufler-setup-defauto-groups ()
  ;; + the bufler-defauto-group macro must be compiled (for subsequent calls to
  ;;   eval-and-compile). This /may/ only affect auto-projectile.
  ;;
  ;; + Overriding bufler-defauto-group may be a bad idea.
  ;;
  ;; + one can not simply define new groups ... or at least not in this manner
  ;;
  ;; + (bufler-defgroups ...) wraps a quasiquoted `(cl-macrolet ... ,@groups)
  ;;   and i am thoroughly confused. there is a handoff to the cl-fdsa "realm"
  ;;   where side effects mostly(?) disappear. are they confined to an obarray?
  ;;   IDK
  (bufler-defauto-group directory
    (propertize (concat "δ· " (file-truename (buffer-local-value 'default-directory buffer)))
                'face 'magit-section-heading))

  (bufler-defauto-group project
    (when-let* ((project (bufler-project-current nil (buffer-local-value 'default-directory buffer)))
                (project-root (bufler-project-root project)))
      (concat "·¶ " project-root)))

  ;; memoize bufler defaults
  (setq dc/bufler-groups-defaults bufler-groups))

;;**** Bufler defgroups

;; TODO: refactor the memoization of dc/bufler-groups-.*
;;
;; + this also seems to lock the bufler-groups in place ... until i (setf
;;   by-reference) .... hmmm
;;
;; (setf bufler-groups dc/bufler-groups-defaults)

(defun dc/bufler-setup-groups ()
  ;; + changed org-roam to org-roam-directory
  ;; + removed projectile group
  ;; + removed helm
  ;; + add auto-tramp
  (setf bufler-groups
        (bufler-defgroups
          ;; Subgroup collecting all named workspaces.
          (group (auto-workspace))
          (group (auto-tramp)
                 (auto-directory)
                 (auto-mode))
          (group
           ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
           (group-or "*Help/Info*"
                     (mode-match "*Help*" (rx bos "help-"))
                     (mode-match "*Info*" (rx bos "info-"))))
          (group
           ;; Subgroup collecting all special buffers (i.e. ones that are not
           ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
           ;; through to other groups, so they end up grouped with their project buffers).
           (group-and
            "*Special*"
            (lambda (buffer)
              (unless (or (funcall (mode-match "Magit" (rx bos "magit-status")) buffer)
                          (funcall (mode-match "Dired" (rx bos "dired")) buffer)
                          (funcall (auto-file) buffer))
                "*Special*")))
           (group
            ;; Subgroup collecting these "special special" buffers
            ;; separately for convenience.
            (name-match "**Special**"
                        (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
           (group
            ;; Subgroup collecting all other Magit buffers, grouped by directory.
            (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
            (auto-directory))
           (auto-mode))
          (dir user-emacs-directory)
          (group
           (dir (if (bound-and-true-p org-roam-directory)
                    org-roam-directory
                  "~/org"))
           (group
            ;; Subgroup collecting indirect Org buffers, grouping them by file.
            ;; This is very useful when used with `org-tree-to-indirect-buffer'.
            (auto-indirect)
            (auto-file))
           ;; Group remaining buffers by whether they're file backed, then by mode.
           (group-not "*special*" (auto-file))
           (auto-mode))
          ;; Subgroup collecting buffers in a version-control project,
          ;; grouping them by directory.
          (group (auto-project)
                 (group-or "*Special*"
                           (mode-match "Magit" (rx bos "magit-status"))
                           (mode-match "Dired" (rx bos "dired")))
                 (auto-mode)
                 ;; (group-not
                 ;;  "*Special*"
                 ;;  (lambda (buffer)
                 ;;    (unless (or (funcall (mode-match "Magit" (rx bos "magit-status")) buffer)
                 ;;                (funcall (mode-match "Dired" (rx bos "dired")) buffer)
                 ;;                (funcall (auto-file) buffer))
                 ;;      "*Special*")))
                 ;; decorates buffers with indication
                 ;; (auto-special) ;; special buffers already consumed
                 ;; (auto-mode) ;; overload
                 ;; (auto-directory ;; overload
                 )
          (group
           (auto-directory)
           (auto-mode))))

  ;; memoize the current settings
  (setq dc/bufler-groups-custom bufler-groups))

;;**** Bufler package

(use-package bufler :straight (:type git :flavor melpa
				     :host github :repo "alphapapa/bufler.el"
				     :files (:defaults (:exclude "helm-bufler.el")
						       "bufler-pkg.el"))
  :init
  (setq bufler-indent-per-level 3)
  :config
  (dc/bufler-setup-defauto-groups)
  (dc/bufler-setup-groups)

  ;; TODO: find better way to advise bufler-select-buffer. combining
  ;; the interactive functions with alternate specs is
  ;; complicated. bufler uses cl-defun specs.
  (advice-add 'bufler-switch-buffer :before #'ace-select-window))

;; bufler:
;; -buffer-mode-annotate-preds
;; -cache-timeout, -cache-related-dirs-p
;; -columns & -column-name-max-width
;; -filter-buffer(-functions,-buffer-mode,/buffer-name-regexp)
;; -filter-buffer-modes
;; -filter-buffer-name-regexp
;; -path-separator
;; -groups
;; -indent-per-level
;; -initial-face-depth
;; -list-(display,group)-buffer-action
;; -mode-hook
;; -bufler-vc-(refresh,remote,state)

;;** Minibuffer

;;*** Minibuffer history

;; TODO: review savehist-file: .emacs.g/var/savehist.el
(use-package savehist :straight (:type built-in)
  :demand t
  :init
  (setq history-length 50
	kill-ring-max 50)

  :config
  (savehist-mode 1)

  ;; Individual history elements can be configured separately
  (put 'minibuffer-history 'history-length 25)
  (put 'kill-ring 'history-length 25))

;;*** Recursive Minibuffers

;; fixes for vertico/consult
(defun dc/crm-indicator (args)
  "Set the indicator for `completing-read-multiple' status with `ARGS'."
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))

(advice-add #'completing-read-multiple :filter-args #'dc/crm-indicator)

(setq enable-recursive-minibuffers t
      minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(add-hook 'window-setup-hook #'minibuffer-depth-indicate-mode)

;;** Look and Feel

;;*** Casual

;;**** casual-info
(use-package casual-info :straight t :demand t :after avy)

;;**** casual-calc
(use-package casual-calc :straight t :demand t :after avy)

;;**** casual-avy
;; + Avy can jump across windows?
;; + Copy functionality is useful
(use-package casual-avy :straight t :demand t :after avy)

;;**** casual-isearch
(use-package casual-isearch :straight t :demand t :after isearch)

;; casual-isearch-tmenu and other symbols aren't loading
;; straight/setup aren't requiring the package (even after eval)

;;*** Themes

;;**** ef-themes

;; the index of the theme in each list doesn't really correspond to its complement
;; - but this would perhaps break in the future anyways.
(defun dc/ef-themes-get-index-for-alternate (theme-sym variant)
    (if-let* ((themes-name (concat "ef-themes-" (symbol-name variant) "-themes"))
              (themes-sym (intern themes-name))
              (themes-list (and (boundp themes-sym)
                                (symbol-value themes-sym))))
        (cl-position theme-sym themes-list)
      (error "dc/ef-themes-get-index: some thing happen.")))

(defun dc/update-face (face1 face2)
  "Swap `face1' with the spec of `face2'."

  ;; TODO: won't survice swapping themes
  (if-let* ((face (get face2 'face))
            (spec (cadar (get face2 'theme-face))))
      (face-spec-set face1 spec)))

(defun dc/ef-themes-get-current-index (theme-sym)
  (let ((all-themes (-interleave ef-themes-dark-themes
                                 ef-themes-light-themes)))
    (/ (cl-position theme-sym all-themes) 2)))

(defun dc/current-theme ()
  ;; (car (reverse custom-enabled-themes))
  ;; nevermind, it's a stack
  (car custom-enabled-themes))

(defun dc/current-theme-is-ef-theme ()
  (equal "ef-" (substring (symbol-name (dc/current-theme)) 0 3)))

(defun dc/current-theme-is-toggled-ef-theme ()
  (and (dc/current-theme-is-ef-theme)
       (memq (car custom-enabled-themes) ef-themes-to-toggle)))

(use-package ef-themes :straight t
  :demand t
  :init
  (setq ef-themes-mixed-fonts t
        ef-themes-to-toggle '(ef-bio ef-cherie))

  ;; i open other emacs +windows+... emacs processes often and i like theme 2
  ;; look distinct so buffers don't start munching each other's files
  ;; (apparently tramp handles this well...  so maybe not a problem))
  :hook
  (emacs-startup-hook . ef-themes-load-random)
  (desktop-after-read-hook . (lambda () (ef-themes-select (car ef-themes-to-toggle)))))

;;*** Pomodoro's

;; from https://github.com/BonfaceKilz/emacs.d
;; pomm integrates with libnotify/polybar, so there's a chance i might use it
;; https://github.com/SqrtMinusOne/pomm.el

;; TODO test to make sure the pomm theme toggling works
;; ... which is EXACTLY why you need TDD/BDD
(defun dc/pomm-toggle-update-theme ()
  ;; don't do anything if it's not my main session
  (if (dc/current-theme-is-toggled-ef-theme)
      (cond
       ((eq (a-get* pomm--state 'current 'kind) 'work)
        ;; (ef-themes-select (dc/current-theme) 'dark)
        (ef-themes--load-theme (car ef-themes-to-toggle)))
       ((eq (a-get* pomm--state 'current 'kind) 'long-break)
        (ef-themes--load-theme (cadr ef-themes-to-toggle)))
       ((eq (a-get* pomm--state 'current 'kind) 'short-break)
        (ef-themes--load-theme (cadr ef-themes-to-toggle))))))

(use-package pomm :straight t
  :demand t
  :init
  (setq pomm-state-file-location
        (expand-file-name "pomm" no-littering-var-directory)
        pomm-third-time-state-file-location
        (expand-file-name "pomm-third-time" no-littering-var-directory))
  :hook
  ((#'dc/pomm-toggle-update-theme #'pomm-update-mode-line-string) . 'pomm-on-status-changed-hook))

;; Other options

;; pomm-csv-history-file ;only creates history CSV when this is set
;; (expand-file-name "pomm.csv" no-littering-var-directory)
;; needs to call "$toolcmd --options " with path/to/audio.wav
;; pomm-audio-executable "toolcmd"
;; pomm-audio-files '(...)
;; pomm-audio-enabled t
;; pomm-audio-tick-enabled t

;;*** Pulse
;; TODO implement with pulse.el: https://blog.meain.io/2020/emacs-highlight-yanked/
;; https://protesilaos.com/emacs/pulsar

;; where hooks are unavailable, append commands to pulsar-pulse-functions
(use-package pulsar :straight t
  :demand t
  :init
  (setq pulsar-pulse t
        pulsar-delay 0.055
        pulsar-iterations 10
        pulsar-face 'pulsar-magenta
        pulsar-highlight-face 'pulsar-yellow)

  ;; changes to pulsar-pulse-functions are effective when pulsar-mode
  ;; loads in a new buffer (the post-command-hooks are buffer-local)
  :config
  (dolist (f '(ace-window
               bufler
               buf-move-up
               buf-move-down
               buf-move-left
               buf-move-right
               xref-find-apropos
               xref-find-definitions
               xref-find-definitions-other-window
               xref-find-definitions-other-frame
               xref-find-definitions-at-mouse
               xref-find-references
               xref-go-back
               xref-go-forward
               ;; TODO eglot
               ;; TODO info
               popper-cycle
               popper-toggle-latest
               popper-toggle-type))
    (add-to-list 'pulsar-pulse-functions f))
  (pulsar-global-mode 1)

  ;; runs on most preview actions

  :hook
  ; runs on imenu selection
  (#'pulsar-recenter-middle . consult-after-jump-hook)
  ((#'pulsar-pulse-line-red) . (prev-error-hook next-error-hook))
  (#'pulsar-pulse-line . window-configuration-change-hook))

;; TODO pulsar-reveal-entry is specific org-mode/outline-mode
;; (:hook pulsar-reveal-entry)
;; (add-to-list 'window-selection-change-functions #'pulsar-reveal-entry)

;;*** Font

(setq-default emojify-display-style 'unicode
              ;; default '(ascii unicode github)
              emojify-emoji-styles '(unicode github))

;; Set the font face based on platform

(defun dc/reset-fonts ()
  (interactive)
  (pcase system-type
    ('gnu/linux
     (set-face-attribute 'default nil
                         :font "Noto Sans Mono"
                         :weight 'regular
                         :height (dw/system-settings-get 'emacs/default-face-size))))

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
                      :font "Noto Sans Mono"
                      :weight 'regular
                      :height (dw/system-settings-get 'emacs/fixed-face-size))

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      ;; :font "Cantarell"
                      :font "Noto Sans"
                      :weight 'light
                      :height (dw/system-settings-get 'emacs/variable-face-size)))

(use-package faces :straight (:type built-in)
  :hook (#'dc/reset-fonts . emacs-startup-hook))

;;*** Window Dividers

;; - requires window-divider-mode being on
;;   - window-divider-default-places t sets to both 'bottom and 'right
;; - M-x customize-face on window-divider to change fgcolor
;;   - any change req. reloading the mode
;;   - changing vertical-border does not req. reload
;; (window-divider-mode +1)
(setq-default window-divider-default-right-width 3
              window-divider-default-bottom-width 3)

;;*** Images

;; my build doesn't have webp ... but just in case
(add-hook 'emacs-startup-hook (lambda () (setq image-types (remq 'webp image-types))))

;;** Editor

(use-package editorconfig :straight t
  :demand t
  :init
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :hook
  (editorconfig-mode . emacs-startup-hook))

;;*** Re-Builder

;; https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder

(use-package re-builder :straight (:type built-in)
  ;; reb-re-syntax: sets the default regexp system (no PCRE available), change
  ;; with C-c TAB.
  ;;
  ;; rx, string, read (deprecated: sregex, lisp-re)
  :init
  (setq-default reb-re-syntax 're))

;;*** Indentation

(setq-default tab-width 2 indent-tabs-mode nil)

;;*** Fills & Alignment

(setq-default fill-column 80)

(use-package display-fill-column-indicator :straight (:type built-in)
  :demand t
  :config
  (global-display-fill-column-indicator-mode))

;;**** Visual Fill Column

;; (use-package visual-line-mode :straight (:type built-in)
;;  :demand t

;; (visual-fill-column :type git :flavor melpa :host codeberg :repo "joostkremers/visual-fill-column")

(use-package visual-fill-column :straight t
  :demand t
  :init
  ;; visual-fill-column in simple.el
  (setq-default visual-fill-column-width nil ; 110
		            visual-fill-column-center-text nil) ;; t

  ;; visual-fill-column-enable-sensible-window-split ;customize values mixed up?

  :hook
  ((org-mode-hook message-mode) . visual-fill-column-mode))

;;*** Selection
(use-package delsel :straight (:type built-in)
  :demand t
  :config
  (delete-selection-mode +1))

;;*** Cursor
(use-package mc :straight t :defer t)


;;**** Clipboard

;; TODO: ensure these variables do not change
;; - can be tested with middle click
;; - https://www.emacswiki.org/emacs/CopyAndPaste#h5o-3


;; x-select vars are obselete
;; x-select-enable-primary nil
;; x-select-enable-clipboard t

(use-package select :straight (:type built-in)
  :demand t
  :init
  (setq-default select-enable-primary nil
		            select-enable-clipboard t))

;; TODO: (setq kill-ring-max 25)

;; if necessary, setup a watch function https://www.gnu.org/software/emacs/manual/html_node/elisp/Watching-Variables.html

;; select-enable-primary defun vterm--set-selection seems to be the only
;; function with a direct reference to this variable but it fucking gets changed
;; all the goddamn time and in doom also. FUCK!
;; - i haven't had the chance to run vterm much, so it's not that.
;; - it it is perhaps related to sleep/hibernate ... or something
;; - vterm-enable-manipulate-selection-data-by-osc52 is nil and this shouldn't run
;; seriously, this is the worst

;;*** Undo

(use-package undo-tree :straight t
  :demand t
  :init
  (setq undo-tree-auto-save-history nil
        undo-tree-mode-lighter "│Ü¿")
  :config
  (global-undo-tree-mode 1))

;;** Highlighting

;; try text-mode, but it may not work well if docs are long ...
;; can't think of why i haven't done this.
(use-package highlight-symbol :straight t
  :demand t
  :init
  (setq highlight-symbol-idle-delay 0.5)
  :hook ((text-mode prog-mode) . highlight-symbol-mode))

;;** Bookmarks

;;*** Burly

;; TODO: set burly-frameset-filter-alist to ensure/prevent some frames are stored
;; accepts filter function

;; TODO:



(use-package burly :straight t :demand t)

;;*** Activities

(use-package activities
  :straight (:type git :flavor melpa
		   :host github :repo "alphapapa/activities.el"))

;; activities-tabs-mode prevents the frame from getting named. more frames
;; gives more options anyways.

;; (:with-hook emacs-startup-hook (:hook activities-tabs-mode))

;; is it possible to sync these to other computers?
;;
;; maybe ... by copying persist/activities-activites, it seems to load
;; activities on the same emacs profile deployed to multiple computers, if all
;; file paths are identical. no idea how this could play out
;;
;; activities get saved to:
;;
;; + serialization: .emacs.g/var/persist/activities-activities
;; + bookmark: .emacs.g/var/bookmarks.el

;; there are three structs:
;;
;; (cl-defstruct activities-buffer (bookmark nil) (filename nil) (name nil) (local-variables nil) (etc nil))
;; (cl-defstruct activities-activity name default last etc)
;; (cl-defstruct activities-activity-state (window-state nil) (etc nil))
;;
;; activities--bufferize-window-state and other functions provide examples of
;; cl-labels to traverse trees (i think?)

;;** UI

(defun dc/forcing-function (msg)
  (interactive)
  (user-error msg))

;; TODO: make interactive (apply-partially #'dc/forcing-function "use M-g instead of C-x pf for #'consult-ripgrep")

;;*** Dired

;; this works, but has some issues & could be cleaned up
;;
;; - tramp buffers look like this
;;
;; ("/ssh:myhost:/data/myproj/worktree/site/_data/_data"
;;  "/ssh:myhost:/data/myproj/worktree/site/_data/site<www2>"
;;  "/ssh:myhost:/data/myproj/worktree/site/_data/www2</ssh:myhost:>")
;;
;; at times, it returns nil when it shouldnt (i think because everything gets
;; filtered out) and i mean the last thing i want to do is make this behavior
;; more unpredictable than it was
;;
;;

;; also, (dear future self, ...) please note that consult/vertico functionality
;; makes this much simpler.
;;
;; + C-r C-s to search history
;; + M-n C-p
;; + C-n C-p
;;
;; C-r C-s would, but copies the current minibuffer value to the new History
;; minibuffer. Apparently I can (kill-whole-line) with C-S-backspace
;;
;; The file system structure on my system is fairly simple to type even for 5-6
;; subdirectories ... but it's still a PITA to clear it out (and cognitive
;; overload to jump tracks to thinking about my file structure when i just want
;; to diff/rename files)

(defun dc/dired-dwim ()
  ;; list open dired buffers in project
  ;;
  ;; if (project-current t), then outside of a project it loops when you select
  ;; "..." choose a dir
  (if-let* ((pr (project-current)))
      (->>
       (project-buffers pr)
       (seq-filter
        (lambda (buffer)
          (let ((name (buffer-name buffer))
                (file (buffer-file-name buffer)))
            (and (or (not (string= (substring name 0 1) " "))
                     file)
                 (not (eq buffer (current-buffer)))
                 (or file (not Buffer-menu-files-only))))))
       ;; (mapcar #'buffer-name)
       (seq-filter
        (lambda (b)
          (eq 'dired-mode (with-current-buffer b major-mode))))
       (mapcar
        (lambda (b)
          (let ((-buffer-file-name (with-current-buffer b default-directory)))
            (expand-file-name (buffer-name b))))))
    (dired-dwim-target-next)))

(use-package dired :straight (:type built-in)
  :demand t
  :init
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash nil

        ;; dired-dwim-target 'dired-dwim-target-recent
        dired-dwim-target #'dc/dired-dwim ;; next window on frame

        ;; NOTE: apparently defaults to: "\\`[.]?#\\|\\`[.][.]?\\'" ...
        dired-omit-files (string-join
                          '("^.DS_Store\\'"
                            "^.project\\(?:ile\\)?\\'"
                            "^.\\(svn\\)\\'"
                            "^.ccls-cache\\'"
                            "\\(?:\\.js\\)?\\.meta\\'"
                            "\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")
                          "\\|")
        ;; dired-dwim-target 'dired-dwim-target-recent
        ;; next window on frame (formerly setq-default)
        dired-dwim-target #'dc/dired-dwim)
  (autoload 'dired-omit-mode "dired-x")
  :hook
  ((hl-line-mode dc/hide-icons-in-guix) . dired-mode-hook))

(use-package recentf :straight (:type built-in)
  :demand t
  :init
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 13
        recentf-menu-filter #'recentf-filter-changer)
  :config
  (add-to-list 'recentf-exclude (rx (and line-start "/gnu/store")))
  :hook
  ;;(window-setup-hook . recentf-mode)
  ;; (recentf-mode +1) ; needs to be enabled later.
  (emacs-startup-hook . recentf-mode))

;; i've been frustrated with dired suggestions for years, but everytime i go to
;; customize it, i have to agree that it's the correct behavior. however, i then
;; seem to forget to keep window state salient when calling dired functions.
;;
;; dired-dwim-target 'dired-dwim-target-recent

;; just to note: the current behavior will cycle through open dired buffers with
;; the below pseudo-logic. it then appends the results to the dired history and
;; youll be at the middle of your dired history with the dwim targets below.

;; (map (λ () (apply is-visible ....)) (windows ⊗ tabs ⊗ frames))

;; press M-n to iterate through the dired-dwim-targets
;; press M-p to iterate through the above dired history

;; keeping this _in mind_ it should work very well, but i have yet to
;; satisfactorily resolve this (i forget to make it a habit... pair programming
;; would have really, really changed my life and 100x my adoption of good emacs
;; habits. it's not that i'm stupid or incompetent. it's that i'm stubborn
;; enough to continue using emacs no matter what while never getting any
;; feedback. no signal, no change. if i don't use emacs, then i'm making the
;; problem of finding people to teach someone harder; i should be the change i
;; wish to see in the world.)

;; anyways, this is a huge  problem if it doesn't work for you.

;;**** wdired-mode

;; For simple renames in directory (avoiding the suggestions)

;; Use M-x wdired-mode: C-x C-q and C-c C-c to save


;;*** Window Management

(use-package avy :straight t)
(use-package ace-window :straight t
  :init
  (setq aw-scope 'frame
        aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

(defun dc/ace-frame-jump (arg)
  "Select `ace-window' with `ARG' using `'visible' scope.  The first
window of each frame will by default be keyed first.  in KDE,
frames either minimized or not on the current desktop do still
get keyed.  in i3, frames in another desktop are still
enumerated.  if they are in the scratchpad, they are enumerated
last even if they're open.  Frames open in a console get keyed,
but can't be jumped to or from."
  (interactive "p")
  (let ((aw-scope 'visible))
    (call-interactively #'ace-window arg)))

(use-package winner :straight (:type built-in)
  :demand t
  :config
  (winner-mode))

;; options including moving buffer without moving pointer
(use-package buffer-move :straight t
  ;; REMOVE: wouldn't load initially
  ;;(:type git :flavor melpa :host github :repo "lukhas/buffer-move")
  :demand t)

;;*** Confirmations

(setq dired-deletion-confirmer 'y-or-n-p
      dired-confirm-shell-command 'y-or-n-p

      ;; dired-no-confirm '()

      ;; files/urs
      openwith-confirm-invocation 'y-or-n-p
      url-confirmation-func 'y-or-n-p
      url-cookie-confirmation 'y-or-n-p
      log-edit-confirm nil
      ;; log-edit-confirm #'yes-or-no-p ; defaults to 'changed

      ;; emacs
      confirm-kill-processes 'y-or-n-p
      confirm-kill-emacs 'yes-or-no-p

      ;; email
      message-confirm-send 'y-or-n-p

      ;; org
      org-table-fix-formulas-confirm nil ;; 'y-or-n-p ; no default is no

      ;; lsp/eglot
      eglot-confirm-server-initiated-edits 'y-or-n-p

      ;; smerge-mode is lazy loaded, default: t
      smerge-change-buffer-confirm t)

;;*** Hydra
(use-package hydra :straight t
  :demand t)

;;*** Timers

;;**** GC Timer

;; TODO: didn't have time to test GC notifications...
;; potential dumb mistakes...

(defvar dc/gc-events-count 0 "Collect GC events")
(defvar dc/gc-notify-interval 0 "Collect GC events")
(defun dc/gc-notify-start (&optional secs)
  (interactive)
  ;; TODO: univ arg
  (let ((secs (or secs 60)))
    (setq-default dc/gc-events-count 0
                  dc/gc-notify-interval secs)
    (unless (memq #'dc/gc-events-inc post-gc-hook)
      (add-hook 'post-gc-hook #'dc/gc-events-inc)
      (run-with-timer secs t #'dc/gc-notify))))

(defun dc/gc-notify-stop ()
  (interactive)
  (when (memq #'dc/gc-events-inc post-gc-hook)
    (remove-hook 'post-gc-hook #'dc/gc-events-inc)
    (cancel-function-timers #'dc/gc-notify)))

(defun dc/gc-events-inc ()
  (set 'dc/gc-events-count (+ dc/gc-events-count 1)))

(defun dc/gc-notify ()
  (when (> dc/gc-events-count 0)
    (alert (format "%s events occured (%s seconds)."
                   dc/gc-events-count
                   dc/gc-notify-interval)
           :title "Emacs GC:")
    (setq-default dc/gc-events-count 0)))

;;**** tmr.el

;; TODO: bind `tmr' and `C-u tmr' to a key
(use-package tmr :straight t :demand t)

(defun dw/tmr-mode-line ()
  (if (not (and (boundp 'tmr--timers)
                tmr--timers))
      ""
    (propertize (format " %s 🕐 %s"
                        (tmr--format-remaining (car tmr--timers))
                        (or (tmr--timer-description (car tmr--timers)) ""))
                'tab-bar '(:foreground "orange"))))

;;*** Doom

;; really hard to live without this one.
(require 'dc-interface-doom)

;; ** Speedbar

(use-package speedbar :straight (:type built-in)
  :init
  (setq speedbar-indentation-width 2
        speedbar-ignored-modes
        '(help-mode
          custom-mode
          eshell-mode
          shell-mode
          term-mode
          vterm-mode
          docker-image-mode
          docker-container-mode
          docker-volume-mode
          docker-network-mode)))

;; ** PDF Tools

(use-package pdf-tools :straight t
  :demand t)

;; (add-to-list 'window-buffer-change-functions #'dc/speedbar-refresh-if-open)
;; (advice-add 'window-change :after #'speedbar-refresh)

;;** UI Components

;;*** Modeline

(column-number-mode)

;;*** Scrollbars

(scroll-bar-mode -1)       ; Disable visible scrollbar

;;*** Fringes

(set-fringe-mode 10)       ; Give some breathing room

;;*** Evaluation Overlays (eros)

(use-package eros :straight t
  :config
  (eros-mode +1))

;;*** Tabs

;; TODO: move mode-line-position to tab-bar
;; (req. tracking state per frame?)

;; see
;; cl-defgeneric project-
;; and cl-defmethod project*
(defun dc/tab-bar-tab-name-project ()
  ;; ....
  )

;;  'current-tab
;;
;; tab-bar-tab-group-face-default

(setq tab-bar-close-button-show nil
      tab-bar-format '(tab-bar-format-history
                       tab-bar-format-tabs-groups
                       tab-bar-separator
                       ;; mode-line-position tab-bar-separator
                       dw/tmr-mode-line
                       tab-bar-separator ; nil
                       ;; display global-mode-string in the tab bar (right-aligned)
                       tab-bar-format-align-right
                       tab-bar-format-global))

;;**** Tabspaces

(use-package tabspaces :straight t
  :init
  (setq-default tabspaces-default-tab "Main"
                ;; NOTE: this remaps switch-to-buffer to the tabspaces command,
                ;; but it's available through C-c TAB b
                ;; tabspaces-use-filtered-buffers-as-default t
                tabspaces-remove-to-default t
                tabspaces-include-buffers '("*scratch*"))
  :config
  (tabspaces-mode 1))

;;**** Tab Management

;; interactive tab/bar commands must be called interactively

;; ((tabs-on-frame (seq-remove (lambda (tab)
;;    (eq (car tab) 'current-tab))
;;    (funcall tab-bar-tabs-function))))

;; (tab-names (mapcar (lambda (tab) (alist-get 'name (cdr tab))) tabs-on-frame))
;; (tab-name (alist-get 'name (nth arg tab-names)))
;; (tab-switch tab-name)

;;**** Switching Tabs

;; NOTE: useful when mapping tabs-to-projects
(defun dw/switch-tab-buffer (&optional arg)
  (interactive "P")
  (cond
   ((and arg (> (car arg) 0)) (call-interactively #'consult-buffer))
   ((project-current) (call-interactively #'project-switch-to-buffer))
   (t (call-interactively #'consult-buffer))))

;; this is definitely "doing it wrong". firefox finally figured out the tabs
;; interface, but we've inculcated bad usage habits into our society -- and
;; these never really change. When there are more than a dozen of tabs in an
;; application's UI, it's time for a new UI
(defun dc/tab-next (&optional arg)
  "Switch to tab by tab number."
  (interactive "P")
  (let* ((tabs-on-frame (funcall tab-bar-tabs-function))
         (num-tabs (length tabs-on-frame)))
    (if (and arg
             (< 3 num-tabs))
        ;; briefly tasing you for forgetting the number of tabs you have open
        (tab-bar-select-tab (mod arg num-tabs))
      (tab-next))))

;; browsers should've made use of shift-tab-n for a long time now, perhaps in
;; some other way.
(defun dc/tab-previous (&optional arg)
  "Switch to tab by tab number."
  (interactive "P")
  (let* ((tabs-on-frame (funcall tab-bar-tabs-function))
         (num-tabs (length tabs-on-frame)))
    (if (and arg
             (< 3 num-tabs))

        (tab-bar-select-tab (+ 1 (mod (- arg) num-tabs)))
      (tab-previous))))


;;** Completion

;;*** Vertico

(setq-default vertico-multiform-categories
              '((bookmark reverse grid)
                (buffer reverse grid)   ; works for ido
                (command reverse)
                (consult-compile-error buffer)
                (consult-compile-multi grid (vertico-grid-annotate . 20))
                ;; (consult-flymake-error)
                (consult-grep buffer)
                (consult-git-log-grep-result buffer)
                (consult-info reverse)
                ;; (consult-kmacro)
                (consult-location buffer)
                ;; (consult-imenu buffer)
                (consult-man reverse grid (vertigo-cycle . t))
                (consult-xref buffer)
                (environment-variable reverse grid)
                (expression reverse)    ; for repeat-complex-command

                (file reverse grid (vertico-grid-annotate . 20))
                ;; (file reverse grid)
                (imenu buffer)
                (info-menu reverse grid)
                (kill-ring reverse grid)
                (minor-mode reverse)
                (consult-org-heading reverse grid)
                ;; (symbol)
                ;; not sure what symbol-help category refers to
                (symbol-help reverse grid (vertico-grid-annotate . 20))
                (theme reverse grid)
                (unicode-name grid reverse)
                (yasnippet grid reverse (vertico-cycle . t))
                (t)))


;;**** Other Completion Categories

;; + color: read-color
;;
;; + tab: consult-buffer-other-tab, tab-bar-select-tab-by-name, dired-other-tab

;; (consult-imenu buffer)

;; fonts need adjustment, causes grid to be misaligned
;; (file grid reverse (vertico-grid-annotate . 20))

;; emojify-insert-emoji uses consult-line
;; (emoji grid)

;; (Man-completion-table ... )
;; (Man reverse grid) ;also doesn't display grid
;; (xref-location grid) ;doesn't display grid

(setq-default vertico-multiform-commands
              '(("flyspell-correct-*" grid reverse (vertico-grid-annotate . 20))
                (org-refile grid reverse indexed)
                (consult-yank-pop indexed)
                ;; (consult-lsp-diagnostics)
                (consult-flycheck reverse)
                (consult-flymake reverse)))

(use-package vertico :straight t
  :demand t
  :custom
  (vertico-cycle t)
  (resize-mini-windows t)
  ;; resize-mini-frames t
  :config
  (vertico-mode)

  ;; TODO: this is still purple
  ;; (custom-set-faces '(vertico-current ((t (:background "#3a3f5a")))))

  ;; To check whether the minor mode is enabled in the current buffer,
  ;; evaluate ‘(default-value 'vertico-multiform-mode)’.

  :hook
  (vertico-directory-tidy . rfn-eshadow-update-overlay-hook)
  ;; from prot's video on files/dired
  ;; to clear out dired suggestions to enter a full path quickly with / or ~/

  ;; different actions for left/right click
  ((vertico-mode vertico-multiform-mode vertico-mouse-mode) . emacs-startup-hook)

  ;; numbers for prefix-based completion,
  ;; handy when toggling vertico-grid-mode
  (vertico-indexed-mode . vertico-mode))

;; TODO: double-check tramp remote configuration
;; https://github.com/minad/vertico/tree/main#tramp-hostname-and-username-completion

(defun vertico-quick-embark (&optional arg)
  "Embark on candidate using quick keys."
  (interactive)
  (when (vertico-quick-jump)
    (embark-act arg)))

;;*** Corfu

;; corfu@0.34 doesn't include most of the corfu-popupinfo changes
(use-package corfu :straight t
  :demand t
  :init
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0.25
        corfu-quit-no-match 'separator
        corfu-quit-at-boundary 'separator
        corfu-excluded-modes '(org-mode markdown-mode)
        corfu-count 15
        corfu-min-width 15
        corfu-popupinfo-max-height 15
        corfu-popupinfo-min-height 5
        corfu-popupinfo-direction 'right
        corfu-popupinfo-delay '(1.0 . 0.0)
        corfu-popupinfo-hide nil     ;don't hide in between transitions
        corfu-preview-current nil)
  :config
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1))

;; straight recipe not found (i'm not using it for now)
;;
;; (use-package corfu-quick :straight t
;;   :after corfu
;;   :init
;;   (setq corfu-quick1 "asdfghjkl;"))


;; the functions that the popup provides are available via corfu
;; - (see dc-keys#corfu-popupinfo-map)
;; - without the popup, the info will be displayed in a temporary buffer
;; - without popupinfo, maintaining terminal-compatibly config is easier

;; corfu-terminal advises functions in corfu-doc which is deprecated
;; (use-package corfu-terminal :straight t)
;;   (unless (display-graphic-p)
;;     (corfu-terminal-mode +1)))

(defun corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))

;;*** Kind Icon

(use-package kind-icon :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;*** Orderless

;; NOTE: see variable docs:
;; - completion-styles
;; - completion-styles-alist
;; - completion-cat
;; - completion-category-overrides (email, eglot

;; completion-styles-alist keys:
;; - external orderless+initialism orderless
;; - emacs21 emacs22 basic partial-completion
;; - substring flex initials shorthand

;; completion-category-defaults keys:
;; - email eglot buffer unicode-name project-file
;; - xref-location info-menu symbol-help

;; NOTE force myself to try initialism)
;; orderless-style-dispatchers '(dc/orderless-first-initialism
;; dc/orderless-regexp)
;; (defun dc/orderless-first-initialism (pattern index _total)
;;   (if (= index 0) 'orderless-initialism))
;; (defun dc/orderless-regexp (pattern index _total)
;;   'orderless-regexp)
;;

(defun dc/match-components-literally ()
  "Components match literally for the rest of the session."
  (interactive)
  (setq-local orderless-matching-styles '(orderless-literal)
              orderless-style-dispatchers nil))

;; TODO: try as a buffer-local variable in various text-mode/prog-mode
;; (setq-default completion-cycle-threshold 29)

;; https://github.com/oantolin/orderless#defining-custom-orderless-styles

(use-package orderless :straight t
  :init

  (setq completion-styles '(orderless basic)
        ;; completion-styles '(orderless+initialism basic)
        orderless-matching-styles '(orderless-prefixes
                                    ;; orderless-initialism
                                    orderless-regexp)
        ;; orderless-style-dispatchers '(orderless-prefixes orderless-regexp)
        ;; these need to be functions
        completion-ignore-case nil
        read-file-name-completion-ignore-case nil
        read-buffer-completion-ignore-case t)

  (setq-default completion-category-overrides
                '((command (styles orderless+initialism))
                  (function (styles orderless+initialism))
                  (symbol (styles orderless+initialism))
                  (variable (styles orderless+initialism))))

  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp))))

;; TODO: enumerating possible keys for completion-category-overrides?
;; TODO: determine whether to add orderless-affix-dispatch-alist
;; adds a nice dynamic matching syntax, but package needs update?
;; https://github.com/oantolin/orderless#component-matching-styles

;;*** WGrep

(use-package wgrep :straight t
  :hook (wgrep-setup . grep-mode-hook))

;;*** Consult

(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult :straight t
  :demand t
  :init
  (setq consult-narrow-key "<"
        ;; consult-preview-key '("C-SPC")
        consult-preview-key '("S-<down>" "S-<up>")

        ;; async timing
        consult-async-min-input 3
        consult-async-input-debounce 0.3
        consult-async-input-throttle 0.5
        ;; < 1.5 * 80
        consult-grep-max-columns 116)
  :config
  (consult-customize
   consult-theme :preview-key nil
   ;; Hide full buffer list by default (use "b" prefix)
   consult--source-buffer :hidden t :default nil)

  ;; CLI tools
  (setq-default completion-in-region-function
                #'dc/completion-in-region-function)
  (setq consult-project-function #'consult--default-project-function
        ;; completion-in-region-function #'consult-completion-in-region

        consult-ripgrep-args
        (string-join (list consult-ripgrep-args dc/ripgrep-args) " "))

  ;;  may need to be set per-mode
  ;; if lsp/lispy/cider/geiser cause problems
  (require 'consult-xref)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name "Workspace Buffers"
          :narrow ?w
          :history 'buffer-name-history
          :category 'buffer
          :state #'consult--buffer-state
          :default t
          :items (lambda () (consult--buffer-query
                             :predicate #'tabspaces--local-buffer-p
                             :sort 'visibility
                             :as #'buffer-name)))
    "Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))


;; TODO: tune consult-preview settings
;; see https://github.com/minad/consult#live-previews
;; consult-preview-key 'any ;useful when calling inside (let ((...)) ...)
;; consult-preview-max-size 10485760
;; consult-preview-raw-size 524288
;; consult-preview-max-count 10
;; consult-preview-excluded-files '(regexp list...)

;; TODO: update the text with the currently selected completion candidate
;; (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)


;; TODO: Optionally make narrowing help available in the minibuffer.
;; You may want to use `embark-prefix-help-command' or which-key instead.
;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

;; consult-narrow-key "C-=" ; doesn't work
;; consult-narrow-key "C-c =" ; not rebound by general-translate-key'

;;**** Consult Minibuffer Fix

;; eval-expression uses (completion-in-region start end coll &optional pred)

(defun dc/completion-in-region-function (&rest args)
  (apply (if vertico-mode
             #'consult-completion-in-region
           #'completion--in-region)
         args))

;;**** Consult Dir

(use-package consult-dir :straight t
  :after consult
  :demand t
  :init
  ;; dc/consult-dir-recentf fails by dynamic/lexical binding issues if consult-dir
  ;; hasn't yet run with the normal value of `consult-dir-sources'.
  (setq-default consult-dir-sources '(consult-dir--source-bookmark
                                       consult-dir--source-default
                                       consult-dir--source-project
                                       consult-dir--source-recentf))

  :config
  (setq consult-dir-project-list-function #'consult-dir-project-dirs))

(defun dc/consult-dir-recentf ()
  "Call `consult-dir' with only the recentf source."
  (interactive)
  (let ((consult-dir-sources '(consult-dir--source-recentf)))
    (consult-dir)))

;; TODO: useful custom consult-dir sources that aren't already simple?

;;**** Consult Flyspell

(use-package consult-flyspell :straight t
  :after flyspell)

;;**** Consult Yasnippet

(use-package consult-yasnippet :straight t
  :after yasnippet
  :config
  (consult-customize consult-yasnippet :preview-key '(:debounce 0.25 any)))

;;**** Consult Magit

(use-package consult-git-log-grep :straight t
  :after consult magit
  :init
  (setq consult-git-log-grep-open-function #'magit-show-commit))

;;**** Consult Recoll

;; full-text search with control over indexing (req. config)
;; https://www.lesbonscomptes.com/recoll/usermanual/usermanual.html
(unless t
  ;; try later
  (use-package consult-recoll :straight t
    :after consult
    :init
    (setq consult-recoll-inline-snippets nil)
    :config
    (consult-customize consult-recoll :preview-key "C-M-m")))

;;**** Consult Codesearch

(defun dc/consult-codesearch-setup ()
  ;; three interactive functions: consult-codesearch,-build-index,find-file
  ;;
  ;; - set CSEARCHINDEX in direnv
  ;; - or `consult-codesearch-csearchindex' in .dir-locals
  (use-package consult-codesearch :straight t))

;;*** Marginalia

(use-package marginalia :straight t
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy
                                marginalia-annotators-light
                                nil))
  :config
  (marginalia-mode))


;; TODO: dc/marginalia-annotators-reset
;; reset marginalia annotators to their default values

;;*** Cape

(defun dc/capf-fix<emacs-29 ()
  (when (< emacs-major-version 29)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)))

(use-package cape :straight t :demand t)

(with-eval-after-load 'cape
  (dc/capf-fix<emacs-29))

;; ignore keyword completion
(defun dc/ignore-elisp-keywords (sym)
  (not (keywordp sym)))

;; TODO capf: use macro and assign this to a key
(defun dc/toggle-emacs-lisp-keyword-completion ()
  (interactive)
  (setq-local completion-at-point-functions
              (list (cape-capf-predicate #'elisp-completion-at-point
                                         #'ignore-elisp-keywords))))

;; TODO: capf: explicit key for completion

;; (keymap-global-set "C-c p e" (cape-interactive-capf #'elisp-completion-at-point))

;; if needed (integrate company completion into capf)
;; (setq-local completion-at-point-functions
;;   (mapcar #'cape-company-to-capf
;;     (list #'company-files #'company-keywords #'company-dabbrev)))

;;*** Embark

;; embark-consult included and loaded with embark
(use-package embark :straight t :demand t
  :config
  ;; Use Embark to show command prefix help
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult :straight t :after (embark consult) :demand t)

;;** TODO: Remove

;; NOTE: this req. doom-modeline and sets faces in the tab-bar
;;   so global-mode-string becomes visible there as well
;;   it's useful for exwm and as an example

;; (defun dw/set-tab-bar-faces ()
;;   ;; TODO setting :background nil warns to set to 'unspecified, but that throws error
;;   (let ((color (face-attribute 'doom-modeline-bar :background nil t)))
;;     (set-face-attribute 'tab-bar-tab nil
;;                         :foreground 'unspecified
;;                         :background 'unspecified
;;                         :weight 'semi-bold
;;                         :underline `(:color ,color)
;;                         :inherit nil)
;;     (set-face-attribute 'tab-bar nil
;;                         :font "Iosevka Aile"
;;                         :foreground 'unspecified
;;                         :inherit 'mode-line)))

;; This never actually gets loaded
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             ;; (dw/set-tab-bar-faces)

;;             (add-to-list 'global-mode-string '(" " display-time-string))
;;             ;; (add-to-list 'global-mode-string '(" " doom-modeline--battery-status))
;;             (add-to-list 'global-mode-string '(" " tracking-mode-line-buffers))

;;             (display-time-mode 1)
;;             (display-battery-mode 1)

;;             (setq-default tab-bar-show t)
;;             (tab-bar-mode 1)
;;             (tab-bar-rename-tab "Main")))

(provide 'dc-interface)

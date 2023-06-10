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

(setq-default fill-column 80)

(scroll-bar-mode -1)       ; Disable visible scrollbar
(tool-bar-mode -1)         ; Disable the toolbar
(set-fringe-mode 10)       ; Give some breathing room

;; there are realgud/dap integrations for inspecting vars/etc with tooltip
(setq tooltip-delay 0.7                 ;2.0
      tooltip-short-delay 0.1)          ;0.5

;; TODO: determine whether there are possible issues with pgtk/wayland?
;; (tooltip-mode -1)       ; Disable tooltips
(tooltip-mode +1)

;; menu-bar-mode gets a bad rap from tool-bar-mode
(menu-bar-mode +1)            ; Enable the menu bar

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      ;; mouse-wheel-scroll-amount '(8)
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse t        ;; scroll window under mouse
      mouse-drag-and-drop-region t
      scroll-step 1 ;; keyboard scroll one line at a time
      use-dialog-box nil ;; Disable dialog boxes since they weren't working in Mac OSX
      ;; inhibit-startup-message t
      visible-bell t)

;; (set-frame-parameter (selected-frame) 'alpha-background 90)
;; (add-to-list 'default-frame-alist '(alpha-background 90))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Use UTF-8 by default
(set-default-coding-systems 'utf-8)

;;(setq large-file-warning-threshold nil)
(setq vc-follow-symlinks t
      ;; "Disable all version control. makes startup and opening files much
      ;; faster except git and svn which I actually use" - jkitchin
      vc-handled-backends '(Git SVN))
(setq ad-redefinition-action 'accept)

;;*** Modeline

(setup (:pkg minions)
  (:hook-into doom-modeline-mode))

(column-number-mode)

(defun dw/start-doom-modeline ()
  (require 'doom-modeline)

  ;; Start it
  (doom-modeline-mode 1)

  ;; Customize the default modeline
  (doom-modeline-def-modeline 'default
    '(bar window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    ;; lsp
    '(objed-state grip debug repl minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))
  (doom-modeline-set-modeline 'default t))

;; guix version does not include the eglot--spinner bugfix
(setup (:pkg doom-modeline :straight t :type git :flavor melpa
             :host github :repo "seagle0128/doom-modeline")
  ;; somehow doom modeline was interfering with after-init-hook's from completing without
  ;; throwing an error. this was inherited properties on faces to be unspecified/undefined
  (:with-hook emacs-startup-hook
    (:hook dw/start-doom-modeline))

  (:option doom-modeline-height (dw/system-settings-get 'emacs/doom-modeline-height)
           doom-modeline-bar-width 6
           ;; doom-modeline-lsp t
           doom-modeline-github nil
           doom-modeline-mu4e nil
           doom-modeline-irc nil
           doom-modeline-minor-modes t
           doom-modeline-persp-name nil
           doom-modeline-buffer-file-name-style 'truncate-except-project
           doom-modeline-major-mode-icon nil)
  (custom-set-faces '(mode-line ((t (:height 1.00))))
                    '(mode-line-inactive ((t (:height 1.00))))))

;;*** Themes
(setup (:pkg ef-themes)
  (:option ef-themes-mixed-fonts t
           ef-themes-to-toggle '(ef-bio ef-cherie))
  ;; i open other emacs windows often and i like theme to look distinct so
  ;; buffers don't start munching each other's files (apparently tramp handles
  ;; this well...  so maybe not a problem))
  (:with-hook emacs-startup-hook (:hook ef-themes-load-random))
  (:with-hook desktop-after-read-hook
    (:hook ;; #'(lambda () (ef-themes-select (car ef-themes-to-toggle)))
     (lambda () (ef-themes-select (car ef-themes-to-toggle))))))

(with-eval-after-load 'ef-themes
  ;; the index of the theme in each list doesn't really correspond to its complement
  ;; - but this would perhaps break in the future anyways.
  (defun dc/ef-themes-get-index-for-alternate (theme-sym variant)
    (if-let* ((themes-name (concat "ef-themes-" (symbol-name variant) "-themes"))
              (themes-sym (intern themes-name))
              (themes-list (and (boundp themes-sym)
                                (symbol-value themes-sym))))
        (cl-position theme-sym themes-list)
      (error "dc/ef-themes-get-index: some thing happen.")))

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
         (memq (car custom-enabled-themes) ef-themes-to-toggle))))

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

(setup (:pkg pomm :straight t)
  (:option pomm-state-file-location
           (expand-file-name "pomm" no-littering-var-directory)
           pomm-third-time-state-file-location
           (expand-file-name "pomm-third-time" no-littering-var-directory))
  ;; (add-hook 'pomm-on-tick-hook 'pomm-update-mode-line-string)
  (add-hook 'pomm-on-status-changed-hook #'dc/pomm-toggle-update-theme)
  (add-hook 'pomm-on-status-changed-hook #'pomm-update-mode-line-string))

;; Other options
;; pomm-csv-history-file ;only creates history CSV when this is set
;; (expand-file-name "pomm.csv" no-littering-var-directory)
;; needs to call "$toolcmd --options " with path/to/audio.wav
;; pomm-audio-executable "toolcmd"
;; pomm-audio-files '(...)
;; pomm-audio-enabled t
;; pomm-audio-tick-enabled t

;;*** Cursor


;;*** Pulse
;; TODO implement with pulse.el: https://blog.meain.io/2020/emacs-highlight-yanked/
;; https://protesilaos.com/emacs/pulsar
(require 'imenu)
(setup (:pkg pulsar)
  (:option pulsar-pulse t
           pulsar-delay 0.055
           pulsar-iterations 10
           pulsar-face 'pulsar-magenta
           pulsar-highlight-face 'pulsar-yellow)
  (:with-hook consult-after-jump-hook   ; runs on most preview actions
    imenu-after-jump-hook               ; runs on imenu selection
    (:hook pulsar-recenter-middle)
    (:hook pulsar-reveal-entry))
  (:with-hook next-error-hook
    (:hook #'pulsar-pulse-line-red))
  ;; TODO pulse on ace-window jump
  (:with-hook window-configuration-change-hook
    (:hook pulsar-reveal-entry))
  ;; (add-to-list 'window-selection-change-functions #'pulsar-reveal-entry)
  (require 'pulsar)
  (pulsar-global-mode 1))

;;*** Font

(setq emojify-display-style 'unicode
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


;; TODO: emacs doesn't seem to call emacs-startup-hook when started by systemd
(dc/reset-fonts)

;; TODO: interactive: set a specific frame's font
;; (set-frame-font)
;; (defun dc/set-frame-font (&rest keys)
;;  (while-let ((next)))
;;  (let ((face ()))))

;; -JB-JetBrains Mono-regular-normal-normal-*-12-*-*-*-d-0-iso10646-1
;; -JB-JetBrains Mono-regular-normal-normal-*-14-*-*-*-d-0-iso10646-1

;;*** Window Dividers
;; - requires window-divider-mode being on
;;   - window-divider-default-places t sets to both 'bottom and 'right
;; - M-x customize-face on window-divider to change fgcolor
;;   - any change req. reloading the mode
;;   - changing vertical-border does not req. reload
;; (window-divider-mode +1)
(setq window-divider-default-right-width 3
      window-divider-default-bottom-width 3)

;;** Editor

;;*** Text
(delete-selection-mode +1)

;;*** Clipbaord
;; TODO: ensure these variables do not change
;; - can be tested with middle click
;; - https://www.emacswiki.org/emacs/CopyAndPaste#h5o-3
(setq select-enable-primary nil
      select-enable-clipboard t
      x-select-enable-primary nil
      x-select-enable-clipboard t)

;; TODO: (setq kill-ring-max 25)

;; if necessary, setup a watch function https://www.gnu.org/software/emacs/manual/html_node/elisp/Watching-Variables.html

;; select-enable-primary defun vterm--set-selection seems to be the only
;; function with a direct reference to this variable but it fucking gets changed
;; all the goddamn time and in doom also. FUCK!
;; - i haven't had the chance to run vterm much, so it's not that.
;; - it it is perhaps related to sleep/hibernate ... or something
;; - vterm-enable-manipulate-selection-data-by-osc52 is nil and this shouldn't run
;; seriously, this is the worst

;;** Highlighting

(setup (:pkg highlight-symbol)
  (:option highlight-symbol-idle-delay 0.5)
  (:hook-into prog-mode))

;;** Bookmarks

;;*** Burly

(setup (:pkg burly))

;;** UI

(defun dc/forcing-function (msg)
  (interactive)
  (user-error msg))

;; TODO: make interactive (apply-partially #'dc/forcing-function "use M-g instead of C-x pf for #'consult-ripgrep")

;;*** Dired
(setup recentf
  (:option recentf-max-saved-items 200
           recentf-max-menu-items 13
           recentf-menu-filter #'recentf-filter-changer))

(with-eval-after-load 'recentf
  (recentf-mode)
  (add-to-list 'recentf-exclude (rx (and line-start "/gnu/store"))))


;;*** Window Management

(setup (:pkg avy))
(setup (:pkg ace-window)
  (:option aw-scope 'frame
           aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
           aw-minibuffer-flag t)
  (ace-window-display-mode 1))

(defun dc/ace-frame-jump ()
  (interactive "P"))

(setup winner
  (winner-mode))

;; options including moving buffer without moving pointer
(setup (:pkg buffer-move))

;;*** Tab Management

;; interactive tab/bar commands must be called interactively

;; ((tabs-on-frame (seq-remove (lambda (tab)
;;    (eq (car tab) 'current-tab))
;;    (funcall tab-bar-tabs-function))))

;; (tab-names (mapcar (lambda (tab) (alist-get 'name (cdr tab))) tabs-on-frame))
;; (tab-name (alist-get 'name (nth arg tab-names)))
;; (tab-switch tab-name)

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

;;*** Confirmations

(setq dired-deletion-confirmer 'y-or-n-p
      ;; dired-no-confirm '()
      url-confirmation-func 'y-or-n-p
      ;; url-cookie-confirmation nil

      ;; smerge-mode is lazy loaded, default: t
      smerge-change-buffer-confirm t)

;;*** Hydra
(setup (:pkg hydra)
  (require 'hydra))

(defhydra dw/smerge-panel ()
  "smerge"
  ("k" (smerge-prev) "prev change" )
  ("j" (smerge-next) "next change")
  ("u" (smerge-keep-upper) "keep upper")
  ("l" (smerge-keep-lower) "keep lower")
  ("q" nil "quit" :exit t))

;;** Completion

;;*** Vertigo

(setup (:pkg vertico)
  (vertico-mode)

  (:option vertico-cycle t
           ;; this seems to be the default for me
           ;; enable-recursive-minibuffers t
           vertico-multiform-categories '((file grid)
                                          (consult-location buffer)
                                          (consult-grep buffer)
                                          (minor-mode reverse)
                                          (imenu buffer)
                                          (t))
           vertico-multiform-commands  '(("flyspell-correct-*" grid reverse)
                                         (org-refile grid reverse indexed)
                                         (consult-yank-pop indexed)
                                         (consult-flycheck)
                                         ;; (consult-lsp-diagnostics)
                                         ))
  (custom-set-faces '(vertico-current ((t (:background "#3a3f5a")))))

  (:with-hook emacs-startup-hook
    ;; different actions for left/right click
    (:hook vertico-mouse-mode)

    ;; numbers for prefix-based completion,
    ;; handy when toggling vertico-grid-mode
    (:hook vertico-indexed-mode)))

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
;; (setq read-extended-command-predicate
;;       #'command-completion-default-include-p)

(defun vertico-quick-embark (&optional arg)
  "Embark on candidate using quick keys."
  (interactive)
  (when (vertico-quick-jump)
    (embark-act arg)))

;;*** Corfu

;; corfu@0.34 doesn't include most of the corfu-popupinfo changes
(setup (:pkg corfu)
  (:option corfu-cycle t
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
  (global-corfu-mode 1))

(setup (:pkg corfu-quick)
  (:option corfu-quick1 "asdfghjkl;"))
;; the functions that the popup provides are available via corfu
;; - (see dc-keys#corfu-popupinfo-map)
;; - without the popup, the info will be displayed in a temporary buffer
;; - without popupinfo, maintaining terminal-compatibly config is easier

;; corfu-terminal advises functions in corfu-doc which is deprecated
;; (setup (:pkg corfu-terminal :straight t)
;;   (unless (display-graphic-p)
;;     (corfu-terminal-mode +1)))

(defun corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))

;;*** Kind Icon

(setup (:pkg kind-icon)
  (:load-after corfu)
  (:option kind-icon-default-face 'corfu-default)
  (:when-loaded
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

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

(setup (:pkg orderless)
  (require 'orderless)
  ;; https://github.com/oantolin/orderless#defining-custom-orderless-styles
  (:option completion-styles '(orderless basic)
           orderless-matching-styles '(orderless-prefixes
                                       ;; orderless-initialism
                                       orderless-regexp)
           ;; orderless-style-dispatchers '(orderless-prefixes orderless-regexp)
           ;; these need to be functions
           completion-ignore-case nil
           read-file-name-completion-ignore-case nil
           read-buffer-completion-ignore-case t)

  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))

  (setq completion-category-overrides
        '((command (styles orderless+initialism))
          (function (styles orderless+initialism))
          (symbol (styles orderless+initialism))
          (variable (styles orderless+initialism)))))

;; TODO: enumerating possible keys for completion-category-overrides?
;; TODO: determine whether to add orderless-affix-dispatch-alist
;; adds a nice dynamic matching syntax, but package needs update?
;; https://github.com/oantolin/orderless#component-matching-styles

;;*** WGrep

(setup (:pkg wgrep)
  (add-hook 'grep-mode-hook #'wgrep-setup))

;;*** Consult

(setup (:pkg consult)
  (require 'consult)
  (:also-load wgrep)

  (defun dw/get-project-root ()
    (when (fboundp 'projectile-project-root)
      (projectile-project-root)))

  ;; TODO: tune consult-preview settings
  ;; see https://github.com/minad/consult#live-previews
  ;; consult-preview-key 'any ;useful when calling inside (let ((...)) ...)
  ;; consult-preview-max-size 10485760
  ;; consult-preview-raw-size 524288
  ;; consult-preview-max-count 10
  ;; consult-preview-excluded-files '(regexp list...)

  ;; TODO: Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; consult-narrow-key "C-=" ; doesn't work
  ;; consult-narrow-key "C-c =" ; not rebound by general-translate-key'
  (:option consult-narrow-key "<f12> ="
           consult-project-root-function #'dw/get-project-root
           completion-in-region-function #'consult-completion-in-region)

  (require 'consult-xref)

  ;;  may need to be set per-mode if lsp/lispy/cider/geiser cause problems
  (:option xref-show-xrefs-function 'consult-xref
           xref-show-definitions-function 'consult-xref))

;;*** Consult Dir

(with-eval-after-load 'consult
  (setup (:pkg consult-dir)
    (:option consult-dir-project-list-function nil)))

(with-eval-after-load 'flyspell
  (setup (:pkg consult-flyspell :straight t :type git :flavor melpa
               :host gitlab :repo "OlMon/consult-flyspell")))

(with-eval-after-load 'yasnippet
  (setup (:pkg consult-yasnippet)
    ;; (:option consult-yasnippet-use-thing-at-point t
    ;;          consult-yasnippet-always-overwrite-thing-at-point t)
    )
  (consult-customize consult-yasnippet :preview-key '(:debounce 0.25)))

(with-eval-after-load 'magit
  (setup (:pkg consult-git-log-grep :straight t :type git :flavor melpa
               :host github :repo "ghosty141/consult-git-log-grep")
    (:option consult-git-log-grep-open-function #'magit-show-commit)))

;;*** Marginalia

(setup (:pkg marginalia)
  (:option marginalia-annotators '(marginalia-annotators-heavy
                                   marginalia-annotators-light
                                   nil))
  (marginalia-mode))

;; TODO: dc/marginalia-annotators-reset
;; reset marginalia annotators to their default values

;;*** Embark

;; embark-consult included and loaded with embark
(setup (:pkg embark)
  ;; Use Embark to show command prefix help
  (setq prefix-help-command #'embark-prefix-help-command))

(provide 'dc-interface)

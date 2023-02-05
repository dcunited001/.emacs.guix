;; -*- lexical-binding: t; -*-

;;* Interface

;;** Basics

(menu-bar-mode +1)            ; Enable the menu bar

;; Thanks, but no thanks
(setq inhibit-startup-message t
      visible-bell t)

(setq-default fill-column 80)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)       ; Give some breathing room

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 1 ;; keyboard scroll one line at a time
      use-dialog-box nil ;; Disable dialog boxes since they weren't working in Mac OSX
      )
      
;; (set-frame-parameter (selected-frame) 'alpha-background 90)
;; (add-to-list 'default-frame-alist '(alpha-background 90))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Use UTF-8 by default
(set-default-coding-systems 'utf-8)

;;(setq large-file-warning-threshold nil)
(setq vc-follow-symlinks t)
(setq ad-redefinition-action 'accept)

;;*** Date & Time

(setq display-time-format "%l:%M %p %b %d W%U"
      display-time-load-average-threshold 0.0)

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
    '(objed-state grip debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))
  (doom-modeline-set-modeline 'default t))

(setup (:pkg doom-modeline)
  (add-hook 'after-init-hook #'dw/start-doom-modeline)
  (:option doom-modeline-height 15
           doom-modeline-bar-width 6
           doom-modeline-lsp t
           doom-modeline-github nil
           doom-modeline-mu4e nil
           doom-modeline-irc nil
           doom-modeline-minor-modes t
           doom-modeline-persp-name nil
           doom-modeline-buffer-file-name-style 'truncate-except-project
           doom-modeline-major-mode-icon nil)
  (custom-set-faces '(mode-line ((t (:height 0.85))))
                    '(mode-line-inactive ((t (:height 0.85))))))


;;*** Themes

(setup (:pkg ef-themes))
(ef-themes-load-random)

;; (setup (:pkg doom-themes))
;; (unless dw/is-termux
;;   ;; TODO: Move this to a system setting
;;   (load-theme
;;    (pcase system-name
;;      ("acidburn" 'doom-ayu-dark)
;;      ("phantom" 'doom-molokai)
;;      (_ 'doom-palenight))
;;    t)
;;   (doom-themes-visual-bell-config))

;;*** Font

;;(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 14)
;;      doom-variable-pitch-font (font-spec :family "Overpass" :size 14)
;;      doom-unicode-font (font-spec :family "JuliaMono" :size 14)
;;      doom-font-increment 2)

;; Set the font face based on platform
(pcase system-type
  ('gnu/linux
   (set-face-attribute 'default nil
                       :font "JetBrains Mono"
                       :weight 'light
                       :height (dw/system-settings-get 'emacs/default-face-size))))

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    :weight 'light
                    :height (dw/system-settings-get 'emacs/fixed-face-size))

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    ;; :font "Cantarell"
                    :font "Overpass"
                    :height (dw/system-settings-get 'emacs/variable-face-size)
                    :weight 'light)



;;** Bookmarks

;;*** Burly

(setup (:pkg burly))

;;** UI

;;*** Hydra
(setup (:pkg hydra)
  (require 'hydra))

;;** Completion

;;;*** Vertigo

(setup (:pkg vertico)
  (vertico-mode)
  (:with-map vertico-map
    (:bind "C-j" vertico-next
           "C-k" vertico-previous
           "C-f" vertico-exit-input))
  (:with-map minibuffer-local-map
    (:bind "M-h" vertico-directory-up))
  (:option vertico-cycle t)
  (custom-set-faces '(vertico-current ((t (:background "#3a3f5a"))))))

;;;*** Corfu

(setup (:pkg corfu)
  (:with-map corfu-map
    (:bind "C-j" corfu-next
           "C-k" corfu-previous
           "TAB" corfu-insert
           [tab] corfu-insert
           "C-f" corfu-insert))
  (:option corfu-cycle t
           corfu-auto t
           corfu-preview-current nil
           corfu-quit-at-boundary t
           corfu-quit-no-match t)
  (global-corfu-mode 1))

;;;*** Kind Icon

(setup (:pkg kind-icon)
  (:load-after corfu)
  (:option kind-icon-default-face 'corfu-default)
  (:when-loaded
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

;;;*** Orderless

(setup (:pkg orderless)
  (require 'orderless)

  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp))))

;;;*** WGrep

(setup (:pkg wgrep)
  (add-hook 'grep-mode-hook #'wgrep-setup))

;;;*** Consult

(setup (:pkg consult)
  (require 'consult)
  (:also-load wgrep)
  (:global "C-s" consult-line
           "C-M-l" consult-imenu)

  (:with-map minibuffer-local-map
    (:bind "C-r" consult-history))

  (defun dw/get-project-root ()
    (when (fboundp 'projectile-project-root)
      (projectile-project-root)))

  (:option consult-project-root-function #'dw/get-project-root
           completion-in-region-function #'consult-completion-in-region))

;;*** ConsultDir

(setup (:pkg consult-dir)
  (:global "C-x C-d" consult-dir)
  (:with-map vertico-map
    (:bind "C-x C-d" consult-dir
           "C-x C-j" consult-dir-jump-file))
  (:option consult-dir-project-list-function nil))

;;*** Marginalia

(setup (:pkg marginalia)
  (:option marginalia-annotators '(marginalia-annotators-heavy
                                   marginalia-annotators-light
                                   nil))
  (marginalia-mode))

;;*** Embark

(setup (:pkg embark)
  (:also-load embark-consult)
  (:global "C-M-." embark-act)
  (:with-map minibuffer-local-map
   (:bind "C-d" embark-act))

  ;; Use Embark to show command prefix help
  (setq prefix-help-command #'embark-prefix-help-command))

(provide 'dc-interface)

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

;;*** Popper

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
                                      "^\\*lsp-ui-imenu"
                                      "^\\*Bufler"
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
(general-def popper-mode-map
  :prefix "M-`"
  "`" 'popper-toggle-latest
  "~" 'popper-cycle
  ;; raise/lower popups
  "M-`" 'popper-toggle-type)

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

;;;*** Vertigo

(setup (:pkg vertico)
  (vertico-mode)
  (:with-map minibuffer-local-map
    (:bind "M-h" vertico-directory-up))
  (:option vertico-cycle t)
  (custom-set-faces '(vertico-current ((t (:background "#3a3f5a"))))))

;;;*** Corfu

(setup (:pkg corfu)
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

  (defun dw/get-project-root ()
    (when (fboundp 'projectile-project-root)
      (projectile-project-root)))

  (:option consult-project-root-function #'dw/get-project-root
           completion-in-region-function #'consult-completion-in-region))

;;*** ConsultDir

(setup (:pkg consult-dir)
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

  ;; Use Embark to show command prefix help
  (setq prefix-help-command #'embark-prefix-help-command))

(provide 'dc-interface)

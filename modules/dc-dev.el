;; -*- lexical-binding: t; -*-

;;** Dev

;;*** Paren Matching

(setup (:pkg smartparens)
  (:hook-into prog-mode))

(setup (:pkg rainbow-delimiters)
  (:hook-into prog-mode))

(setup (:pkg rainbow-mode)
  (:hook-into org-mode
              emacs-lisp-mode
              web-mode
              typescript-mode
              js2-mode))

;;*** Buffer Environments

(setup (:pkg buffer-env)
  (:option buffer-env-script-name "manifest.scm")
  (add-hook 'comint-mode-hook #'hack-dir-local-variables-non-file-buffer)
  (add-hook 'hack-local-variables-hook #'buffer-env-update))

;;*** M-x compile

(setup compile
  (:option compilation-scroll-output t))

(setq compilation-environment '("TERM=xterm-256color"))

(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

(defun dw/auto-recompile-buffer ()
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
    (add-hook 'after-save-hook #'recompile nil t)))

;;*** project.el

(defun dw/current-project-name ()
  (file-name-nondirectory
   (directory-file-name
    (project-root (project-current)))))

(defun dw/switch-project-action ()
  (interactive)
  (let* ((project-name (dw/current-project-name))
         (tab-bar-new-tab-choice #'magit-status)
         (tab-index (tab-bar--tab-index-by-name project-name)))
    (if tab-index
        (tab-bar-select-tab (1+ tab-index))
      (tab-bar-new-tab)
      (tab-bar-rename-tab project-name))))

(defun dw/close-project-tab ()
  (interactive)
  (let* ((project-name (dw/current-project-name))
         (tab-index (tab-bar--tab-index-by-name project-name)))
    (project-kill-buffers t)
    (when tab-index
      (tab-bar-close-tab (1+ tab-index)))))

(setup (:pkg project)
  (:global "C-M-p" project-find-file)
  (:with-map project-prefix-map
    (:bind "k" dw/close-project-tab)
    (:bind "F" consult-ripgrep))

  (setq project-switch-commands #'magit-status))

;;*** Eglot

(setup (:pkg eglot)
  ;; TODO: Don't load until needed
  (require 'eglot)
  (define-key eglot-mode-map (kbd "C-c C-a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c C-r") #'eglot-rename)
  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil)
  ;; TODO: Is this needed now?
  (add-to-list 'eglot-server-programs
               '((js2-mode typescript-mode) . ("typescript-language-server" "--stdio"))))

(with-eval-after-load 'eglot
  (add-hook 'c-mode-hook 'eglot-ensure))

;;*** Magit

(setup (:pkg magit)
  (:also-load magit-todos)
  (:global "C-M-;" magit-status)
  (:option magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (setup (:pkg magit-todos))

;;*** Git

(setup (:pkg git-link)
  (:option git-link-open-in-browser t))

;;*** Code Formatting

(setup (:pkg apheleia)
  (apheleia-global-mode +1))

(setup (:pkg lispy)
  (:hook #'turn-off-smartparens-mode)
  (:hook-into emacs-lisp-mode
              scheme-mode
              ielm-mode
              scheme-mode
              ;; racket-mode
              ;; hy-mode
              ;; lfe-mode
              ;; dune-mode
              ;; fennel-mode
              clojure-mode))

;;*** Emacs Lisp

(setup emacs-lisp-mode
  (:hook flycheck-mode))

;;*** Common Lisp

(setup (:pkg sly)
  (:disabled)
  (:file-match "\\.lisp\\'"))

;;*** Scheme

;; Include .sld library definition files
(setup (:pkg scheme-mode)
  (:file-match "\\.sld\\'"))

(setup (:pkg geiser)
  ;; (setq geiser-default-implementation 'gambit)
  ;; (setq geiser-active-implementations '(gambit guile))
  ;; (setq geiser-implementations-alist '(((regexp "\\.scm$") gambit)
  ;;                                      ((regexp "\\.sld") gambit)))
  ;; (setq geiser-repl-default-port 44555) ; For Gambit Scheme
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-repl-default-port 44555) ; For Gambit Scheme
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile))))

;;*** Mesche

(setup mesche
  (:load-path "~/Projects/Code/mesche/mesche-emacs")
  (:with-mode mesche-mode
    (:file-match "\\.msc\\'"))
  (require 'mesche))

;;*** Snippets
(setup (:pkg emmet)
  (:hook-into sgml-mode css-mode))

(setup (:pkg yasnippet)
  (:load-after emmet)
  (require 'yasnippet)
  (require 'doom-snippets)
  ;; doom-snippets-dir
  ;; doom-snippets-enable-short-helpers t
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (yas-reload-all))


(provide 'dc-dev)
